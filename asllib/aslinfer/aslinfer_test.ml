open Asllib
open AST

type fail_reason =
  | Unsupported of string   (* lower.ml hit an unsupported construct *)
  | TypeError               (* aslref type-checking failed *)
  | ParseError              (* aslref parsing failed *)
  | VerificationError       (* Textual SIL verification failed *)
  | OtherError of string    (* unexpected error *)

type test_result =
  | Pass
  | Fail of fail_reason

type expectedness =
  | ExpectedPositive
  | ExpectedNegative

type classified_result = {
  file : string;
  expectedness : expectedness;
  result : test_result;
}

let string_of_fail_reason = function
  | Unsupported msg -> Printf.sprintf "Unsupported(%s)" msg
  | TypeError -> "TypeError"
  | ParseError -> "ParseError"
  | VerificationError -> "VerificationError"
  | OtherError msg -> Printf.sprintf "Other(%s)" msg

let string_of_result = function
  | Pass -> "PASS"
  | Fail reason -> Printf.sprintf "FAIL - %s" (string_of_fail_reason reason)

let starts_with s prefix =
  let n = String.length prefix in
  String.length s >= n && String.sub s 0 n = prefix

let ends_with s suffix =
  let n = String.length s in
  let m = String.length suffix in
  n >= m && String.sub s (n - m) m = suffix

let contains_substring s needle =
  let n = String.length s in
  let m = String.length needle in
  let rec loop i =
    if i + m > n then false
    else if String.sub s i m = needle then true
    else loop (i + 1)
  in
  m = 0 || loop 0

(* --- Expected-negative test detection --- *)

let is_expected_negative_test file =

  let base = Filename.basename file in

  ends_with base ".bad.asl"

  || contains_substring base ".bad"

  || contains_substring base "-bad"

  || starts_with base "bad-"

  || starts_with base "bad_"

  || starts_with base "bad."

  || contains_substring file "/parser-errors.t/"

  || contains_substring file "/CNegative"

  || contains_substring file "/TNegative"

let expectedness_of_file file =
  if is_expected_negative_test file then ExpectedNegative else ExpectedPositive

let string_of_expectedness = function
  | ExpectedPositive -> "positive"
  | ExpectedNegative -> "expected-negative"

let expected_negative_succeeded = function
  | Fail ParseError | Fail TypeError -> true
  | _ -> false

(* let positive_succeeded = function
  | Pass -> true
  | _ -> false *)

(* --- Arguments --- *)

type args = {
  files : string list;
  output_file : string;
  quiet : bool;
  verify : bool;
  show_expected_negative : bool;
  dump_ast_shapes : bool;
}

let parse_args () =
  let files = ref [] in
  let output_file = ref "" in
  let quiet = ref false in
  let verify = ref false in
  let show_expected_negative = ref false in
  let dump_ast_shapes = ref false in

  let speclist = [
    ("-o", Arg.Set_string output_file,
     " Output file for results");
    ("-q", Arg.Set quiet,
     " Only print positive-test failures and unexpected expected-negative results");
    ("--verify", Arg.Set verify,
     " Also run Textual verification on successful lowerings");
    ("--show-expected-negative", Arg.Set show_expected_negative,
     " Print expected-negative test results too");
    ("--dump-ast-shapes", Arg.Set dump_ast_shapes,
     " Print AST constructor shapes for typed AST before lowering");
  ] in

  let prog =
    if Array.length Sys.argv > 0 then Filename.basename Sys.argv.(0)
    else "aslinfer_test"
  in

  let usage_msg =
    Printf.sprintf
      "Test runner for the ASL-to-Infer frontend.\n\n\
       Infer parses Sys.argv during module initialization.  Put aslinfer_test\n\
       arguments after \"--\" so Infer ignores them.\n\n\
       USAGE:\n\
       \t%s [INFER_FLAGS] -- [OPTIONS] [TARGETS]\n\n\
       EXAMPLES:\n\
       \t%s -- -q --verify asllib/tests\n\
       \tdune exec asllib/aslinfer/aslinfer_test.exe -- -- -q asllib/tests\n"
      prog prog
  in

  (* IBase.Config / CLOpt may inspect Sys.argv at module initialization time.
     Infer stops parsing at "--", so our own test-runner args must come after
     "--". *)
  let our_args =
    let rec after_dd = function
      | [] -> []
      | "--" :: rest -> rest
      | _ :: rest -> after_dd rest
    in
    after_dd (Array.to_list Sys.argv)
  in

  let our_argv = Array.of_list (prog :: our_args) in

  let anon_fun f = files := f :: !files in

  begin
    try
      Arg.parse_argv our_argv (Arg.align speclist) anon_fun usage_msg
    with
    | Arg.Bad msg ->
      Printf.eprintf "%s\n%!" msg;
      exit 1
    | Arg.Help msg ->
      Printf.printf "%s\n%!" msg;
      exit 0
  end;

  let files = List.rev !files in

  if files = [] then begin
    Printf.eprintf "%s: no ASL files or directories specified\n%!" prog;
    Printf.eprintf "%s\n%!" usage_msg;
    exit 1
  end;

  let ensure_exists s =
    if Sys.file_exists s then ()
    else begin
      Printf.eprintf "%s: cannot find %S\n%!" prog s;
      exit 1
    end
  in

  List.iter ensure_exists files;

  {
    files;
    output_file = !output_file;
    quiet = !quiet;
    verify = !verify;
    show_expected_negative = !show_expected_negative;
    dump_ast_shapes = !dump_ast_shapes;
  }

(* --- Type-checker with infer mode --- *)

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

module ShapeTbl = Map.Make(String)

let bump key tbl =
  ShapeTbl.update key
    (function None -> Some 1 | Some n -> Some (n + 1))
    tbl

let string_of_slice = function
  | Slice_Single _ -> "Slice_Single"
  | Slice_Range _ -> "Slice_Range"
  | Slice_Length _ -> "Slice_Length"
  | Slice_Star _ -> "Slice_Star"

let shape_of_expr (e : AST.expr) =
  match e.desc with
  | E_Literal _ -> "E_Literal"
  | E_Var _ -> "E_Var"
  | E_Binop _ -> "E_Binop"
  | E_Unop _ -> "E_Unop"
  | E_Call _ -> "E_Call"
  | E_Cond _ -> "E_Cond"
  | E_GetField _ -> "E_GetField"
  | E_Arbitrary _ -> "E_Arbitrary"
  | E_Tuple _ -> "E_Tuple"
  | E_GetArray _ -> "E_GetArray"
  | E_GetItem _ -> "E_GetItem"
  | E_ATC _ -> "E_ATC"
  | E_Slice _ -> "E_Slice"
  | E_Record _ -> "E_Record"
  | E_Pattern _ -> "E_Pattern"
  | E_GetFields _ -> "E_GetFields"
  | E_GetEnumArray _ -> "E_GetEnumArray"
  | E_EnumArray _ -> "E_EnumArray"
  | E_Array _ -> "E_Array"
  | E_GetCollectionFields _ -> "E_GetCollectionFields"

let shape_of_ty (ty : AST.ty) =
  match ty.desc with
  | T_Int _ -> "T_Int"
  | T_Bits _ -> "T_Bits"
  | T_Bool -> "T_Bool"
  | T_String -> "T_String"
  | T_Real -> "T_Real"
  | T_Tuple _ -> "T_Tuple"
  | T_Array _ -> "T_Array"
  | T_Record _ -> "T_Record"
  | T_Exception _ -> "T_Exception"
  | T_Named _ -> "T_Named"
  | _ -> "unknown-type"

let shape_of_lexpr (le : AST.lexpr) =
  match le.desc with
  | LE_Discard -> "LE_Discard"
  | LE_Var _ -> "LE_Var"
  | LE_SetField _ -> "LE_SetField"
  | LE_SetArray _ -> "LE_SetArray"
  | LE_Slice _ -> "LE_Slice"
  | LE_SetFields _ -> "LE_SetFields"
  | LE_Destructuring _ -> "LE_Destructuring"
  | LE_SetEnumArray _ -> "LE_SetEnumArray"
  | LE_SetCollectionFields _ -> "LE_SetCollectionFields"

let rec collect_expr_shapes tbl (e : AST.expr) =
  let tbl =
    match e.desc with
    | E_Slice (base, slices) ->
      let key =
        "E_Slice base=" ^ shape_of_expr base
        ^ " slices="
        ^ String.concat "+"
            (List.map string_of_slice slices)
      in
      bump key tbl
    | E_GetField (base, _field) ->
      bump ("E_GetField base=" ^ shape_of_expr base) tbl
    | E_GetFields (base, fields) ->
      bump
        (Printf.sprintf "E_GetFields base=%s fields=%d"
           (shape_of_expr base) (List.length fields))
        tbl
    | E_Pattern _ ->
      bump "E_Pattern" tbl
    | E_ATC (_, ty) ->
      bump ("E_ATC ty=" ^ shape_of_ty ty) tbl
    | _ ->
      tbl
  in
  match e.desc with
  | E_Binop (_, a, b) ->
    collect_expr_shapes (collect_expr_shapes tbl a) b
  | E_Unop (_, a)
  | E_ATC (a, _)
  | E_GetField (a, _)
  | E_GetFields (a, _)
  | E_Slice (a, _) ->
    collect_expr_shapes tbl a
  | E_Call call ->
    let call_args = call.args [@warning "-42"] in
    List.fold_left collect_expr_shapes tbl call_args
  | E_Tuple args -> 
    List.fold_left collect_expr_shapes tbl args
  | E_Array _ ->
    tbl
  | E_Cond (c, a, b) ->
    collect_expr_shapes
      (collect_expr_shapes (collect_expr_shapes tbl c) a)
      b
  | E_GetArray (a, i) ->
    collect_expr_shapes (collect_expr_shapes tbl a) i
  | E_GetItem (a, _) ->
    collect_expr_shapes tbl a
  | E_Record (_, fields) ->
    List.fold_left
      (fun tbl (_, e) -> collect_expr_shapes tbl e)
      tbl fields
  | _ ->
    tbl

and collect_stmt_shapes tbl (s : AST.stmt) =
  let tbl =
    match s.desc with
    | S_Assign (le, e) ->
      collect_expr_shapes (collect_lexpr_shapes tbl le) e
    | S_Decl (_, _, _, Some e) ->
      collect_expr_shapes tbl e
    | S_Return (Some e)
    | S_Assert e ->
      collect_expr_shapes tbl e
    | S_Call call ->
      let call_args = call.args [@warning "-42"] in
      List.fold_left collect_expr_shapes tbl call_args

    | S_Print print ->
      List.fold_left collect_expr_shapes tbl print.args
    | _ ->
      tbl
  in
  match s.desc with
  | S_Seq (a, b) ->
    collect_stmt_shapes (collect_stmt_shapes tbl a) b
  | S_Cond (c, a, b) ->
    collect_stmt_shapes
      (collect_stmt_shapes (collect_expr_shapes tbl c) a)
      b
  | S_While (c, _, body) ->
    collect_stmt_shapes (collect_expr_shapes tbl c) body
  | S_Repeat (body, c, _) ->
    collect_expr_shapes (collect_stmt_shapes tbl body) c
  | S_For { start_e; end_e; body; _ } ->
    collect_stmt_shapes
      (collect_expr_shapes (collect_expr_shapes tbl start_e) end_e)
      body
  | S_Try (body, catchers, otherwise) ->
    let tbl = collect_stmt_shapes tbl body in
    let tbl =
      List.fold_left
        (fun tbl (_, _, body) -> collect_stmt_shapes tbl body)
        tbl catchers
    in
    (match otherwise with
     | None -> tbl
     | Some body -> collect_stmt_shapes tbl body)
  | _ ->
    tbl

and collect_lexpr_shapes tbl (le : AST.lexpr) =
  let tbl =
    match le.desc with
    | LE_Slice (base, slices) ->
      bump
        ("LE_Slice base=" ^ shape_of_lexpr base
         ^ " slices="
         ^ String.concat "+"
             (List.map string_of_slice slices))
        tbl

    | LE_SetFields (base, fields, _something) ->
      bump
        (Printf.sprintf "LE_SetFields base=%s fields=%d"
          (shape_of_lexpr base) (List.length fields))
        tbl

    | LE_SetCollectionFields (_base, fields, _something) ->
      bump
        (Printf.sprintf "LE_SetCollectionFields base=identifier fields=%d"
          (List.length fields))
        tbl

    | _ ->
      tbl
  in

  match le.desc with
  | LE_SetField (base, _) ->
    collect_lexpr_shapes tbl base

  | LE_SetArray (base, _) ->
    collect_lexpr_shapes tbl base

  | LE_Slice (base, _) ->
    collect_lexpr_shapes tbl base

  | LE_SetFields (base, _, _) ->
    collect_lexpr_shapes tbl base

  | LE_SetCollectionFields (_, _, _) ->
    tbl

  | LE_Destructuring les ->
    List.fold_left collect_lexpr_shapes tbl les

  | _ ->
    tbl

(* --- Run one test --- *)

let run_test ~verify ~dump_ast_shapes file : test_result =
  try
    (* Parse *)
    let ast =
      try Builder.from_file `ASLv1 file
      with _ -> raise (Failure "parse")
    in

    let ast = Builder.with_stdlib ~no_stdlib0:true ast in

    (* Type-check *)
    let typed_ast, _env =
      try type_check_for_infer ast
      with _ -> raise (Failure "typecheck")
    in

    
    if dump_ast_shapes then begin
      let tbl =
        List.fold_left
          (fun tbl (d : AST.decl) ->
            match d.desc with
            | D_Func f ->
              (match f.body with
              | SB_ASL body -> collect_stmt_shapes tbl body
              | SB_Primitive _ -> tbl)
            | _ -> tbl)
          ShapeTbl.empty
          typed_ast
      in
      ShapeTbl.iter
        (fun key count ->
          Printf.printf "AST_SHAPE %5d %s %s\n%!" count file key)
        tbl
    end;

    (* Lower to Textual *)
    let sil =
      try Aslinfer_lib.Lower.lower_program file typed_ast
      with
      | Failure msg when starts_with msg "Unsupported:" ->
        let detail =
          String.sub msg
            (String.length "Unsupported:")
            (String.length msg - String.length "Unsupported:")
          |> String.trim
        in
        raise (Failure ("unsupported:" ^ detail))
      | Failure msg ->
        raise (Failure ("lowering:" ^ msg))
    in

    (* Optionally verify *)
    if verify then begin
      match Textuallib.TextualVerification.verify_strict sil with
      | Error _ -> raise (Failure "verification")
      | Ok _ -> ()
    end;

    Pass
  with
  | Aslinfer_lib.Lower.Unsupported u ->
    Fail (Unsupported (Aslinfer_lib.Lower.string_of_unsupported u))
  | Aslinfer_lib.Lower.Lowering_error e ->
    Fail (OtherError (Aslinfer_lib.Lower.string_of_lowering_error e))
  | Failure msg when starts_with msg "parse" ->
    Fail ParseError
  | Failure msg when starts_with msg "typecheck" ->
    Fail TypeError
  | Failure msg when starts_with msg "verification" ->
    Fail VerificationError
  | Failure msg when starts_with msg "unsupported:" ->
    let detail =
      String.sub msg
        (String.length "unsupported:")
        (String.length msg - String.length "unsupported:")
    in
    Fail (Unsupported detail)
  | Failure msg when starts_with msg "lowering:" ->
    let detail =
      String.sub msg
        (String.length "lowering:")
        (String.length msg - String.length "lowering:")
    in
    Fail (OtherError ("Lowering: " ^ detail))
  | exn ->
    Fail (OtherError (Printexc.to_string exn))

(* --- File collection --- *)

let rec dir_contents = function
  | [] -> []
  | f :: fs when Sys.is_directory f ->
    let children =
      Sys.readdir f |> Array.to_list
      |> List.map (Filename.concat f)
    in
    dir_contents (children @ fs)
  | f :: fs ->
    f :: dir_contents fs

let collect_asl files =
  dir_contents files
  |> List.filter (fun f -> Filename.extension f = ".asl")
  |> List.sort String.compare

(* --- Summary --- *)

type summary = {
  total : int;
  pass : int;
  unsupported : int;
  typeerror : int;
  parseerror : int;
  verification : int;
  other : int;
}

let empty_summary = {
  total = 0;
  pass = 0;
  unsupported = 0;
  typeerror = 0;
  parseerror = 0;
  verification = 0;
  other = 0;
}

let update_summary s = function
  | Pass ->
    { s with total = s.total + 1; pass = s.pass + 1 }
  | Fail (Unsupported _) ->
    { s with total = s.total + 1; unsupported = s.unsupported + 1 }
  | Fail TypeError ->
    { s with total = s.total + 1; typeerror = s.typeerror + 1 }
  | Fail ParseError ->
    { s with total = s.total + 1; parseerror = s.parseerror + 1 }
  | Fail VerificationError ->
    { s with total = s.total + 1; verification = s.verification + 1 }
  | Fail (OtherError _) ->
    { s with total = s.total + 1; other = s.other + 1 }

let pass_percent s =
  100.0 *. float_of_int s.pass /. float_of_int (max 1 s.total)

let string_of_summary title s =
  Printf.sprintf
    "\n=== %s ===\n\
     Total:          %d\n\
     PASS:           %d (%.1f%%)\n\
     Unsupported:    %d\n\
     Type error:     %d\n\
     Parse error:    %d\n\
     Verification:   %d\n\
     Other:          %d\n"
    title
    s.total
    s.pass
    (pass_percent s)
    s.unsupported
    s.typeerror
    s.parseerror
    s.verification
    s.other

type expected_negative_summary = {
  en_total : int;
  en_expected_failure : int;
  en_unexpected_pass : int;
  en_unexpected_unsupported : int;
  en_unexpected_verification : int;
  en_unexpected_other : int;
}

let empty_expected_negative_summary = {
  en_total = 0;
  en_expected_failure = 0;
  en_unexpected_pass = 0;
  en_unexpected_unsupported = 0;
  en_unexpected_verification = 0;
  en_unexpected_other = 0;
}

let update_expected_negative_summary s result =
  match result with
  | Fail ParseError | Fail TypeError ->
    { s with
      en_total = s.en_total + 1;
      en_expected_failure = s.en_expected_failure + 1 }
  | Pass ->
    { s with
      en_total = s.en_total + 1;
      en_unexpected_pass = s.en_unexpected_pass + 1 }
  | Fail (Unsupported _) ->
    { s with
      en_total = s.en_total + 1;
      en_unexpected_unsupported = s.en_unexpected_unsupported + 1 }
  | Fail VerificationError ->
    { s with
      en_total = s.en_total + 1;
      en_unexpected_verification = s.en_unexpected_verification + 1 }
  | Fail (OtherError _) ->
    { s with
      en_total = s.en_total + 1;
      en_unexpected_other = s.en_unexpected_other + 1 }

let string_of_expected_negative_summary s =
  Printf.sprintf
    "\n=== Expected-negative ASLRef tests ===\n\
     Total:                    %d\n\
     Expected parse/type fail:  %d\n\
     Unexpected PASS:           %d\n\
     Unsupported in lower.ml:   %d\n\
     Verification error:        %d\n\
     Other:                     %d\n"
    s.en_total
    s.en_expected_failure
    s.en_unexpected_pass
    s.en_unexpected_unsupported
    s.en_unexpected_verification
    s.en_unexpected_other

let reached_lowering = function
  | Pass
  | Fail (Unsupported _)
  | Fail VerificationError
  | Fail (OtherError _) ->
      true
  | Fail TypeError
  | Fail ParseError ->
      false

let result_counts_for_positive results =
  List.fold_left
    (fun s cr ->
      match cr.expectedness with
      | ExpectedPositive -> update_summary s cr.result
      | ExpectedNegative -> s)
    empty_summary
    results

let result_counts_for_positive_reaching_lowering results =
  List.fold_left
    (fun s cr ->
      match cr.expectedness with
      | ExpectedPositive when reached_lowering cr.result ->
        update_summary s cr.result
      | ExpectedPositive
      | ExpectedNegative ->
        s)
    empty_summary
    results

let result_counts_for_all results =
  List.fold_left
    (fun s cr -> update_summary s cr.result)
    empty_summary
    results

let result_counts_for_expected_negative results =
  List.fold_left
    (fun s cr ->
      match cr.expectedness with
      | ExpectedNegative -> update_expected_negative_summary s cr.result
      | ExpectedPositive -> s)
    empty_expected_negative_summary
    results

(* --- Top unsupported grouping --- *)

let split_on_substring s needle =
  let len_s = String.length s in
  let len_n = String.length needle in
  let rec search i =
    if i + len_n > len_s then None
    else if String.sub s i len_n = needle then Some i
    else search (i + 1)
  in
  if len_n = 0 then Some 0 else search 0

let first_line s =
  match String.index_opt s '\n' with
  | None -> s
  | Some i -> String.sub s 0 i

let unsupported_key msg =
  let msg = first_line msg |> String.trim in
  (* lower.ml messages look like:
       expr E_Slice at file.asl:line:col: pretty expression
     Group by the stable prefix before " at ". *)
  match split_on_substring msg " at " with
  | Some i -> String.sub msg 0 i
  | None -> msg

let top_unsupported_positive results =
  let tbl = Hashtbl.create 128 in

  List.iter
    (fun cr ->
      match cr.expectedness, cr.result with
      | ExpectedPositive, Fail (Unsupported msg) ->
        let key = unsupported_key msg in
        let count, sample_msg, sample_file =
          match Hashtbl.find_opt tbl key with
          | Some (count, sample_msg, sample_file) ->
            count, sample_msg, sample_file
          | None ->
            0, msg, cr.file
        in
        Hashtbl.replace tbl key (count + 1, sample_msg, sample_file)
      | _ -> ())
    results;

  Hashtbl.fold
    (fun key (count, sample_msg, sample_file) acc ->
      (key, count, sample_msg, sample_file) :: acc)
    tbl []
  |> List.sort (fun (_, a, _, _) (_, b, _, _) -> compare b a)

let take n xs =
  let rec aux acc n xs =
    if n <= 0 then List.rev acc
    else match xs with
      | [] -> List.rev acc
      | x :: rest -> aux (x :: acc) (n - 1) rest
  in
  aux [] n xs

let string_of_top_unsupported_positive results =
  let items = top_unsupported_positive results in
  let limit = 40 in

  match items with
  | [] -> ""
  | _ ->
    let lines =
      take limit items
      |> List.map (fun (key, count, sample_msg, sample_file) ->
        Printf.sprintf
          "  %5d  %-35s  example: %s\n         %s"
          count key sample_file sample_msg)
    in
    "\n=== Top unsupported lowering cases, positive tests only ===\n"
    ^ String.concat "\n" lines
    ^ "\n"

(* --- Rendering results --- *)

let should_print_result args cr =
  match args.quiet, cr.expectedness, cr.result with
  | false, ExpectedPositive, _ ->
    true
  | false, ExpectedNegative, _ ->
    args.show_expected_negative
  | true, ExpectedPositive, Pass ->
    false
  | true, ExpectedPositive, Fail _ ->
    true
  | true, ExpectedNegative, result ->
    (* In quiet mode, hide expected parse/type failures unless explicitly asked,
       but always show expected-negative tests that behave unexpectedly. *)
    args.show_expected_negative || not (expected_negative_succeeded result)

let string_of_classified_result cr =
  Printf.sprintf "%s [%s]: %s"
    (string_of_result cr.result)
    (string_of_expectedness cr.expectedness)
    cr.file

let string_of_unexpected_expected_negative results =
  let unexpected =
    List.filter
      (fun cr ->
        match cr.expectedness with
        | ExpectedPositive -> false
        | ExpectedNegative -> not (expected_negative_succeeded cr.result))
      results
  in
  match unexpected with
  | [] -> ""
  | _ ->
    let lines =
      List.map
        (fun cr -> "  " ^ string_of_classified_result cr)
        unexpected
    in
    "\n=== Expected-negative tests with unexpected result ===\n"
    ^ String.concat "\n" lines
    ^ "\n"

let string_of_positive_verification_errors results =
  let verification =
    List.filter
      (fun cr ->
        match cr.expectedness, cr.result with
        | ExpectedPositive, Fail VerificationError -> true
        | _ -> false)
      results
  in
  match verification with
  | [] -> ""
  | _ ->
    let lines =
      List.map (fun cr -> "  " ^ cr.file) verification
    in
    "\n=== Positive tests with Textual verification errors ===\n"
    ^ String.concat "\n" lines
    ^ "\n"

(* --- Main --- *)

let run_all args =
  let files = collect_asl args.files in

  if files = [] then begin
    Printf.eprintf "No .asl files found in targets: %s\n%!"
      (String.concat ", " args.files);
    exit 1
  end;

  let results =
    List.map
      (fun file ->
        {
          file;
          expectedness = expectedness_of_file file;
          result =
            run_test
              ~verify:args.verify
              ~dump_ast_shapes:args.dump_ast_shapes
              file;
        })
      files
  in

  let lines =
    results
    |> List.filter (should_print_result args)
    |> List.map string_of_classified_result
  in

  let all_summary = result_counts_for_all results in
  let positive_summary = result_counts_for_positive results in
  let positive_lowering_summary =
  result_counts_for_positive_reaching_lowering results
  in
  let expected_negative_summary =
    result_counts_for_expected_negative results
  in

  let output =
    String.concat "\n" lines
    ^ "\n"
    ^ string_of_summary "All tests, raw result counts" all_summary
    ^ string_of_summary "Positive tests, all frontend+lowering results" positive_summary
    ^ string_of_summary "Positive tests reaching lower.ml" positive_lowering_summary
    ^ string_of_expected_negative_summary expected_negative_summary
    ^ string_of_top_unsupported_positive results
    ^ string_of_positive_verification_errors results
    ^ string_of_unexpected_expected_negative results
  in

  if args.output_file = "" then
    print_string output
  else begin
    let chan = open_out args.output_file in
    output_string chan output;
    close_out chan;
    Printf.printf "Results written to %s\n" args.output_file;
    print_string
    (string_of_summary "Positive tests reaching lower.ml"
      positive_lowering_summary);
    print_string
      (string_of_expected_negative_summary expected_negative_summary)
  end

let () =
  let args = parse_args () in
  run_all args