open Textual_ast
open Format

let rec pp_exp fmt = function
  | Var id -> fprintf fmt "%s" id
  | Lvar name -> fprintf fmt "&%s" name
  | Const_int n -> fprintf fmt "%d" n
  | Load e -> fprintf fmt "load %a" pp_exp e
  | Call (proc, args) ->
    fprintf fmt "%s(%a)" proc
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_exp) args
  | Not e -> fprintf fmt "!%a" pp_exp e

let pp_instr fmt = function
  | Assign { id; typ; rhs = Load e } ->
    fprintf fmt "    %s: %s = load %a" id typ pp_exp e
  | Assign { id; rhs; _ } ->
    fprintf fmt "    %s = %a" id pp_exp rhs
  | Store { lvar; value; typ } ->
    fprintf fmt "    store &%s <- %a : %s" lvar pp_exp value typ
  | Prune e ->
    fprintf fmt "    prune %a" pp_exp e

let pp_term fmt = function
  | Ret e -> fprintf fmt "    ret %a" pp_exp e
  | Jmp labels ->
    fprintf fmt "    jmp %s" (String.concat ", " labels)

let pp_node fmt node =
  fprintf fmt "  #%s:@\n" node.label;
  List.iter (fun i -> pp_instr fmt i; fprintf fmt "@\n") node.instrs;
  pp_term fmt node.term;
  fprintf fmt "@\n"

let pp_proc fmt proc =
  fprintf fmt "define %s(%a) : %s {@\n"
    proc.name
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ")
       (fun fmt (n, t) -> fprintf fmt "%s: %s" n t))
    proc.params
    proc.ret_type;
  List.iter (fun (n, t) -> fprintf fmt "  local %s: %s@\n" n t) proc.locals;
  List.iter (pp_node fmt) proc.nodes;
  fprintf fmt "}@\n"

let pp_module fmt m =
  fprintf fmt ".source_language = \"%s\"@\n@\n" m.source_lang;
  List.iter (fun p -> pp_proc fmt p; fprintf fmt "@\n") m.procs
