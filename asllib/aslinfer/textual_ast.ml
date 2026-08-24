type exp =
  | Var of string
  | Lvar of string
  | Const_int of int
  | Load of exp
  | Call of string * exp list
  | Not of exp

type instr =
  | Assign of { id: string; typ: string; rhs: exp }
  | Store of { lvar: string; value: exp; typ: string }
  | Prune of exp

type terminator =
  | Ret of exp
  | Jmp of string list

type node = {
  label: string;
  instrs: instr list;
  term: terminator;
}

type proc = {
  name: string;
  params: (string * string) list;
  ret_type: string;
  locals: (string * string) list;
  nodes: node list;
}

type sil_module = {
  source_lang: string;
  procs: proc list;
}
