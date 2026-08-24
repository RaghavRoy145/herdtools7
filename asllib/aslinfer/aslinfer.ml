open Asllib

let type_check_for_infer ast =
  let module C = struct
    let check = Typing.TypeCheck
    let output_format = Error.HumanReadable
    let print_typed = false
    let use_field_getter_extension = false
    let fine_grained_side_effects = false
    let use_conflicting_side_effects_extension = false
    let override_mode = Typing.Permissive
    let infer_mode = true
  end in
  let module T = Typing.Annotate (C) in
  T.type_check_ast ast

(* --- Argument parsing ---
   IBase.Config parses Sys.argv at module init time (CLOpt.parse in Config.ml).
   Any unrecognized arguments cause errors before main runs. CLOpt stops at "--",
   so we extract everything after "--" and parse it ourselves with Arg. *)

let filename = ref ""
let outfile = ref None
let capture_mode = ref false
let debug_mode = ref false

let usage = "aslinfer [INFER_FLAGS] -- FILE.asl [OPTIONS]"

let speclist = [
  ("-o",       Arg.String (fun s -> outfile := Some s), "FILE  Write Textual SIL to FILE");
  ("--capture", Arg.Set capture_mode,                   "      Direct capture to Infer DB");
  ("--debug",  Arg.Set debug_mode,                      "      Print Textual SIL to stderr");
]

let () =
  let argv = Array.to_list Sys.argv in
  let our_args =
    let rec after_dd = function
      | [] -> []
      | "--" :: rest -> rest
      | _ :: rest -> after_dd rest
    in
    after_dd argv
  in
  let our_argv = Array.of_list ("aslinfer" :: our_args) in
  (try Arg.parse_argv our_argv (Arg.align speclist)
       (fun s -> filename := s) usage
   with Arg.Bad msg | Arg.Help msg ->
     Format.eprintf "%s@." msg; Stdlib.exit 1);
  if String.length !filename = 0 then begin
    Arg.usage (Arg.align speclist) usage;
    Stdlib.exit 1
  end;
  let ast = Builder.from_file `ASLv1 !filename in
  let ast = Builder.with_stdlib ~no_stdlib0:true ast in
  let typed_ast, _env = type_check_for_infer ast in
  let sil = Aslinfer_lib.Lower.lower_program !filename typed_ast in
  if !debug_mode then begin
    Format.eprintf "--- Textual SIL ---@.";
    Textuallib.Textual.Module.pp Format.err_formatter sil;
    Format.eprintf "@.--- End Textual SIL ---@."
  end;
  match !capture_mode with
  | true ->
    IBase.ResultsDir.create_results_dir () ;
    IBase.ResultsDir.RunState.add_run_to_sequence () ;
    IBase.DBWriter.start () ;
    Aslinfer_lib.Lower.capture_to_db sil ;
    Format.printf "Captured to DB.@."
  | false ->
    let fmt = match !outfile with
      | Some path -> Format.formatter_of_out_channel (open_out path)
      | None -> Format.std_formatter
    in
    Textuallib.Textual.Module.pp fmt sil;
    Format.pp_print_flush fmt ()