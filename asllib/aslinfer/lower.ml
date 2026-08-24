open Asllib
open AST
open ASTUtils

(*

   Current limitations of ASL -> Textual lowering:
   - ASL integers and booleans are lowered as Textual integers. It does not preserve all ASL
     numeric refinements.
   - ASL bitvectors are also represented as integers. Width-sensitive bitvector
     operators are emitted as named helper calls; those helpers preserve the
     operation boundary but are opaque to later analysis unless modeled there.
   - ASL records, exceptions, and tuples are value structs in Textual.  Any use
     of an address is a Textual storage/field-access requirement, not an ASL
     pointer semantic.
   - Unsupported constructs raise Unsupported.
*)

module T = Textuallib.Textual

type unsupported_kind =
  | UnsupportedExpr
  | UnsupportedStmt
  | UnsupportedLexpr
  | UnsupportedTy
  | UnsupportedDecl
  | UnsupportedBinop
  | UnsupportedUnop

type unsupported = {
  kind : unsupported_kind;
  tag : string;
  loc : Lexing.position option;
  detail : string;
}

type lowering_error =
  | TextualVerificationFailed
  | TextualToSilFailed
  | InternalLoweringError of string

let string_of_lowering_error = function
  | TextualVerificationFailed -> "TextualVerificationFailed"
  | TextualToSilFailed -> "TextualToSilFailed"
  | InternalLoweringError s -> "InternalLoweringError: " ^ s

exception Unsupported of unsupported
exception Lowering_error of lowering_error

(* --- Helpers --- *)

(* Convert an OCaml lexer position from ASL parsing into a Textual source
   location. Textual locations are used by the verifier and by diagnostics. *)
let loc_of_pos (p : Lexing.position) : T.Location.t =
  T.Location.known ~line:p.pos_lnum ~col:(p.pos_cnum - p.pos_bol)

(* Fallback location used for synthetic nodes/declarations where there is no
   single ASL source position. *)
let loc_unknown = T.Location.Unknown

(* --- Unsupported reporting --- *)

(* Produce a stable file:line:column string for coverage reports. *)
let pos_string (p : Lexing.position) =
  Printf.sprintf "%s:%d:%d"
    p.pos_fname
    p.pos_lnum
    (p.pos_cnum - p.pos_bol)

(* Central helper for unsupported lowering diagnostics.

   The format:
     Unsupported: <kind> <constructor> at <location>: <detail>

   The runner groups these strings to show top unsupported constructs. *)
let unsupported_here kind tag pos fmt =
  Format.kasprintf
    (fun detail ->
      raise (Unsupported {
        kind;
        tag;
        loc = Some pos;
        detail = String.trim detail;
      }))
    fmt

(* Typed wrappers around unsupported_here. *)
let unsupported_expr tag (e : AST.expr) fmt =
  unsupported_here UnsupportedExpr tag e.pos_start fmt

let unsupported_stmt tag (s : AST.stmt) fmt =
  unsupported_here UnsupportedStmt tag s.pos_start fmt

let unsupported_lexpr tag (le : AST.lexpr) fmt =
  unsupported_here UnsupportedLexpr tag le.pos_start fmt

let unsupported_ty tag (ty : AST.ty) fmt =
  unsupported_here UnsupportedTy tag ty.pos_start fmt

let unsupported_decl tag (d : AST.decl) fmt =
  unsupported_here UnsupportedDecl tag d.pos_start fmt

let unsupported_binop tag =
  raise (Unsupported {
    kind = UnsupportedBinop;
    tag;
    loc = None;
    detail = "";
  })

let unsupported_unop tag =
  raise (Unsupported {
    kind = UnsupportedUnop;
    tag;
    loc = None;
    detail = "";
  })

let string_of_unsupported_kind = function
  | UnsupportedExpr -> "expr"
  | UnsupportedStmt -> "stmt"
  | UnsupportedLexpr -> "lexpr"
  | UnsupportedTy -> "type"
  | UnsupportedDecl -> "decl"
  | UnsupportedBinop -> "binop"
  | UnsupportedUnop -> "unop"

let string_of_unsupported u =
  let loc =
    match u.loc with
    | None -> ""
    | Some p -> " at " ^ pos_string p
  in
  if u.detail = "" then
    Printf.sprintf "%s %s%s"
      (string_of_unsupported_kind u.kind) u.tag loc
  else
    Printf.sprintf "%s %s%s: %s"
      (string_of_unsupported_kind u.kind) u.tag loc u.detail

(* Human-readable names for ASL binops.

   This function is diagnostics: it does not decide which operators are
   supported. Unsupported operators should still be reported with their exact
   ASL constructor name so coverage output tells us what to implement next.

   *)
let string_of_binop : AST.binop -> string = function
  | `AND -> "AND"
  | `BAND -> "BAND"
  | `BEQ -> "BEQ"
  | `BOR -> "BOR"

  | `DIV -> "DIV"
  | `DIVRM -> "DIVRM"
  | `MOD -> "MOD"
  | `RDIV -> "RDIV"

  | `XOR -> "XOR"
  | `OR -> "OR"
  | `BIC -> "BIC"
  | `BV_CONCAT -> "BV_CONCAT"
  | `STR_CONCAT -> "STR_CONCAT"

  | `EQ -> "EQ"
  | `NE -> "NE"
  | `GT -> "GT"
  | `GE -> "GE"
  | `LT -> "LT"
  | `LE -> "LE"

  | `IMPL -> "IMPL"

  | `SUB -> "SUB"
  | `MUL -> "MUL"
  | `ADD -> "ADD"
  | `POW -> "POW"

  | `SHL -> "SHL"
  | `SHR -> "SHR"

(* Constructor-name helpers for unsupported expression coverage. *)
let string_of_expr_desc (e : AST.expr) =
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

(* Constructor-name helpers for unsupported statement coverage. *)
let string_of_stmt_desc (s : AST.stmt) =
  match s.desc with
  | S_Pass -> "S_Pass"
  | S_Seq _ -> "S_Seq"
  | S_Return _ -> "S_Return"
  | S_Decl _ -> "S_Decl"
  | S_Assert _ -> "S_Assert"
  | S_Cond _ -> "S_Cond"
  | S_While _ -> "S_While"
  | S_Repeat _ -> "S_Repeat"
  | S_For _ -> "S_For"
  | S_Call _ -> "S_Call"
  | S_Throw _ -> "S_Throw"
  | S_Try _ -> "S_Try"
  | S_Assign _ -> "S_Assign"
  | S_Print _ -> "S_Print"
  | S_Pragma _ -> "S_Pragma"
  | S_Unreachable -> "S_Unreachable"

(* Constructor-name helpers for unsupported left-hand side expressions. *)
let string_of_lexpr_desc (le : AST.lexpr) =
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

(* Type-name helper used in diagnostics. At the moment, type lowering is partial
   and most types collapse to int unless a struct/tuple/array case is special. *)
let string_of_ty_desc (ty : AST.ty) =
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

(* Build a top-level Textual procedure name. ASL subprograms are not class
   methods, so the enclosing class is TopLevel. *)
let mk_procname name =
  { T.QualifiedProcName.enclosing_class = TopLevel;
    name = T.ProcName.of_string name;
    metadata = None }

(* Textual types can carry attributes. This frontend currently emits no
   extra type attributes, so this helper keeps all type construction uniform. *)
let mk_typ_annotated ty =
  T.Typ.{ typ = ty; attributes = [] }

(* Build an unconditional multi-target jump terminator. Conditional branching in
   Textual is represented as jump-to-several-successors plus Prune instructions
   at the target blocks. *)
let mk_jump labels =
  T.Terminator.Jump
    (List.map (fun lbl -> T.Terminator.{ label = lbl; ssa_args = [] }) labels)

(* Every lowered procedure starts at a single entry block. *)
let entry_label = T.NodeName.of_string "entry"

(* --- Binop / Unop lowering via Textuallib typed API --- *)

(* Map supported ASL binary operators to Textual's builtin SIL operator
   procedure declarations.

   Important: some ASL operators have semantics that depend on whether operands
   are booleans, integers, or bitvectors (overloading). Current mappings are
   limited and should be audited before adding more operators. *)
let sil_of_binop : AST.binop -> T.QualifiedProcName.t = function
  | `ADD -> T.ProcDecl.of_binop (IR.Binop.PlusA None)
  | `SUB -> T.ProcDecl.of_binop (IR.Binop.MinusA None)
  | `MUL -> T.ProcDecl.of_binop (IR.Binop.Mult None)
  | `DIV -> T.ProcDecl.of_binop IR.Binop.DivI
  | `MOD -> T.ProcDecl.of_binop IR.Binop.Mod
  | `EQ  -> T.ProcDecl.of_binop IR.Binop.Eq
  | `NE  -> T.ProcDecl.of_binop IR.Binop.Ne
  | `LT  -> T.ProcDecl.of_binop IR.Binop.Lt
  | `LE  -> T.ProcDecl.of_binop IR.Binop.Le
  | `GT  -> T.ProcDecl.of_binop IR.Binop.Gt
  | `GE  -> T.ProcDecl.of_binop IR.Binop.Ge
  | `AND -> T.ProcDecl.of_binop IR.Binop.BAnd
  | `OR  -> T.ProcDecl.of_binop IR.Binop.BOr
  | `XOR -> T.ProcDecl.of_binop IR.Binop.BXor
  | `BAND -> T.ProcDecl.of_binop IR.Binop.LAnd
  | `BOR  -> T.ProcDecl.of_binop IR.Binop.LOr
  | `SHL -> T.ProcDecl.of_binop IR.Binop.Shiftlt
  | `SHR -> T.ProcDecl.of_binop IR.Binop.Shiftrt
  | op -> unsupported_binop (string_of_binop op)

(* Map supported ASL unary operators to Textual builtin unary operators. *)
let sil_of_unop : AST.unop -> T.QualifiedProcName.t = function
  | NEG  -> T.ProcDecl.of_unop IR.Unop.Neg
  | BNOT -> T.ProcDecl.of_unop IR.Unop.LNot
  | NOT  -> T.ProcDecl.of_unop IR.Unop.BNot

(* --- Builder: Fields use Textuallib types --- *)

(* Mutable lowering state for a single ASL function/procedure.

   Textual is block-based. We incrementally append instructions to the current
   block and finalize a block with terminate. The builder also tracks local type
   information that is needed for field accesses and tuple extraction. *)
type builder = {
  (* Next SSA temporary id. Textual identifiers n0, n1, ... come from here. *)
  mutable next_id: int;

  (* Next synthetic block label suffix. *)
  mutable next_label: int;

  (* Instructions accumulated for the current basic block. *)
  mutable instrs: T.Instr.t list;

  (* Completed Textual nodes for the current procedure. *)
  mutable nodes: T.Node.t list;

  (* Label of the block currently being filled. *)
  mutable current_label: T.NodeName.t;

  (* Local variables declared in the Textual procedure. *)
  mutable locals: (T.VarName.t * T.Typ.annotated) list;

  (* True after current_label has been emitted as a completed node. Used to avoid
     adding dead instructions after return/throw. *)
  mutable terminated: bool;

  (* Last source location seen during lowering. Used for terminator locations. *)
  mutable last_loc: T.Location.t;

  (* Ad-hoc map from ASL variable name to lowered struct name. Used for records
     and helper tuple structs. This is not a full type environment. *)
  mutable var_types: (string * string) list;

  (* Exceptional successors for the current block. S_Try temporarily installs a
     catch block here so Throw terminators can connect to it. *)
  mutable exn_succs: T.NodeName.t list;

  (* Current function formal arguments and their ASL types. Used to recover
     record parameter types for field loads/stores. *)
  mutable func_args: (string * AST.ty) list;

  (* Name of the function currently being lowered; useful for future diagnostics. *)
  mutable current_func: string;

  (* Textual result type of the current function.  Statement lowering consults
     this for void/non-void returns and noreturn helper paths. *)
  mutable result_type: T.Typ.t;
}

let create_builder () = {
  next_id = 0; next_label = 0;
  instrs = []; nodes = [];
  current_label = entry_label;
  locals = []; terminated = false;
  last_loc = loc_unknown;
  var_types = [];
  exn_succs = [];
  func_args = [];
  current_func = "";
  result_type = T.Typ.Void;
}

(* Record the lowered struct type for an ASL variable. *)
let add_var_type b name type_name =
  b.var_types <- (name, type_name) :: b.var_types

let get_var_struct_type b name =
  List.assoc_opt name b.var_types

(* Allocate a fresh Textual SSA identifier. *)
let fresh_id b =
  let id = T.Ident.of_int b.next_id in
  b.next_id <- b.next_id + 1;
  id

(* Allocate a fresh Textual block label with a readable prefix. *)
let fresh_label b prefix =
  let l = T.NodeName.of_string (Printf.sprintf "%s_%d" prefix b.next_label) in
  b.next_label <- b.next_label + 1;
  l

(* Create a fresh local variable name. *)
let fresh_varname b prefix =
  let v = T.VarName.of_string (Printf.sprintf "%s_%d" prefix b.next_id) in
  b.next_id <- b.next_id + 1;
  v

(* Append one instruction to the current block. *)
let emit b i =
  b.instrs <- b.instrs @ [i]

(* Finish the current block with the given terminator. After this, start_block
   must be called before emitting more instructions. *)
let terminate b term =
  b.nodes <- b.nodes @ [{
    T.Node.label = b.current_label;
    ssa_parameters = [];
    exn_succs = b.exn_succs;
    instrs = b.instrs;
    last = term;
    last_loc = b.last_loc;
    label_loc = b.last_loc;
  }];
  b.instrs <- [];
  b.terminated <- true

(* Start filling a new basic block. *)
let start_block b label =
  b.current_label <- label;
  b.instrs <- [];
  b.terminated <- false

(* Add an integer local if not already present. This is the default local type;
   structured/array locals are inserted directly where their type is known. *)
let add_local b (vn : T.VarName.t) =
  if not (List.exists (fun (v, _) -> T.VarName.equal v vn) b.locals) then
    b.locals <- b.locals @ [(vn, mk_typ_annotated T.Typ.Int)]

let add_local_typed b (vn : T.VarName.t) (typ : T.Typ.t) =
  if not (List.exists (fun (v, _) -> T.VarName.equal v vn) b.locals) then
    b.locals <- b.locals @ [(vn, mk_typ_annotated typ)]

let varname_of_ident name = T.VarName.of_string name

(* --- Record/struct support --- *)

let mk_typename name = T.TypeName.of_string name

(* Textual fields are qualified by the enclosing struct type. *)
let mk_fieldname struct_name field_name : T.qualified_fieldname =
  { enclosing_class = mk_typename struct_name;
    name = T.FieldName.of_string field_name }

(* These flags request synthetic helper declarations. Helpers are added only if
   a lowered program uses them.

   Note: these declarations have no bodies here.  They are appropriate
   for primitives whose exact Textual encoding is not implemented yet, but they
   are opaque summaries from this frontend's point of view. *)
let _needs_arbitrary = ref false
let _needs_assert_failure = ref false
let _needs_unreachable = ref false
let _needs_slice_helper = ref false
let _needs_bv_concat_helper = ref false
let _needs_pow_helper = ref false
let _needs_divrm_helper = ref false
let _needs_rdiv_helper = ref false
let _needs_str_concat_helper = ref false
let _needs_bv_slice_length_helper = ref false
let _needs_bv_slice_range_helper = ref false
let _needs_bv_slice_single_helper = ref false
let _needs_bv_slice_multi_helper = ref false
let _needs_bv_update_length_helper = ref false
let _needs_bv_update_range_helper = ref false
let _needs_bv_update_single_helper = ref false
let _needs_bv_update_multi_helper = ref false
let _needs_bool_impl_helper = ref false

(* Tuple value structs are named __tuple_N with fields item0 ... itemN-1.
   They model ASL tuple values *)
let tuple_type_name n = mk_typename (Printf.sprintf "__tuple_%d" n)

let tuple_field n i =
  mk_fieldname (Printf.sprintf "__tuple_%d" n) (Printf.sprintf "item%d" i)

(* Set of tuple arities used in the module. lower_program emits struct decls for
   all arities in this table. *)
let _emitted_tuple_types : (int, bool) Hashtbl.t = Hashtbl.create 8

(* Map sorted record field names -> declared type name. This is a lightweight
   way to recover the name of anonymous-looking record types where the AST only
   gives us fields at the use site. *)
let _type_decl_names : (string list, string) Hashtbl.t = Hashtbl.create 16

(* Original declarations for named types.  A T_Named is a Textual struct only if
   it aliases a record/exception declaration; aliases for ints/bits/enums must
   lower to their scalar representation to keep function signatures verifiable. *)
let _type_decl_defs : (string, AST.ty) Hashtbl.t = Hashtbl.create 32

(* Map (struct-name, field-name) to the original ASL field type. *)
let _struct_field_types : ((string * string), AST.ty) Hashtbl.t = Hashtbl.create 64

let _defined_procs : ((string * int), unit) Hashtbl.t = Hashtbl.create 64
let _external_calls : ((string * int), unit) Hashtbl.t = Hashtbl.create 64
let _external_return_tuple_arity : ((string * int), int) Hashtbl.t =
  Hashtbl.create 64
let _defined_globals : (string, unit) Hashtbl.t =
  Hashtbl.create 64
let _func_return_tuple_arity : (string, int) Hashtbl.t =
  Hashtbl.create 64

let _external_globals : (string, unit) Hashtbl.t =
  Hashtbl.create 64
let _external_return_tuple_arity : ((string * int), int) Hashtbl.t =
  Hashtbl.create 64

let _expected_call_return_tuple_arity : int option ref = ref None

let register_type_decls (ast : AST.t) =
  List.iter (fun (d : AST.decl) ->
    match d.desc with
    | D_TypeDecl (name, ty, _) ->
      Hashtbl.replace _type_decl_defs name ty;
      (match ty.desc with
       | T_Record fields | T_Exception fields ->
         let key = List.map fst fields |> List.sort String.compare in
         Hashtbl.replace _type_decl_names key name;
         List.iter
           (fun (field_name, field_ty) ->
             Hashtbl.replace _struct_field_types (name, field_name) field_ty)
           fields
       | _ -> ())
    | _ -> ()
  ) ast

let find_struct_name_for_record fields =
  let key = List.map fst fields |> List.sort String.compare in
  Hashtbl.find_opt _type_decl_names key

let lookup_field_ty struct_name field_name =
  Hashtbl.find_opt _struct_field_types (struct_name, field_name)

let named_type_is_struct name =
  match Hashtbl.find_opt _type_decl_defs name with
  | Some ty ->
    (match ty.desc with
     | T_Record _ | T_Exception _ -> true
     | _ -> false)
  | None ->
    (* Unknown named types are usually external/user-defined records. *)
    true

let tuple_struct_name n =
  Printf.sprintf "__tuple_%d" n

let tuple_struct_typ n =
  T.Typ.Struct (tuple_type_name n)

let tuple_fieldname n i =
  tuple_field n i

let add_tuple_local b vn n =
  add_local_typed b vn (tuple_struct_typ n)

let load_tuple_field_from_lvar b base_lvar n index loc =
  let id = fresh_id b in
  emit b (T.Instr.Load {
    id;
    exp =
      T.Exp.Field {
        exp = T.Exp.Lvar base_lvar;
        field = tuple_fieldname n index;
      };
    typ = Some T.Typ.Int;
    loc;
  });
  T.Exp.Var id

let materialize_tuple_value_to_lvar b n value loc =
  let tmp = fresh_varname b "_tuple_base" in
  add_tuple_local b tmp n;
  emit b (T.Instr.Store {
    exp1 = T.Exp.Lvar tmp;
    typ = Some (tuple_struct_typ n);
    exp2 = value;
    loc;
  });
  tmp

(* Emit one helper struct declaration for a tuple arity. *)
let tuple_struct_decl n : T.Module.decl =
  let fields = List.init n (fun i ->
    T.FieldDecl.{
      qualified_name = tuple_field n i;
      typ = T.Typ.Int;
      attributes = [];
    }) in
  T.Module.Struct T.Struct.{
    name = tuple_type_name n;
    supers = [];
    fields;
    attributes = [];
  }

(* Lower ASL types to Textual types.

   Scalars and bitvectors currently use the
   integer abstraction.  Tuples, named records, and exceptions are value structs.
   If an anonymous record cannot be matched to a declared struct, callers should
   either synthesize a struct explicitly or keep the construct unsupported. *)

let rec lower_ty (ty : AST.ty) : T.Typ.t =
  match ty.desc with
  | T_Tuple ts ->
    let n = List.length ts in
    Hashtbl.replace _emitted_tuple_types n true;
    tuple_struct_typ n
  | T_Array _ ->
    T.Typ.Array T.Typ.Int
  | T_Named n ->
    (match Hashtbl.find_opt _type_decl_defs n with
     | Some aliased_ty ->
       (match aliased_ty.desc with
        | T_Record _ | T_Exception _ -> T.Typ.Struct (mk_typename n)
        | _ -> lower_ty aliased_ty)
     | None -> T.Typ.Struct (mk_typename n))
  | T_Record fields | T_Exception fields ->
    (match find_struct_name_for_record fields with
     | Some n -> T.Typ.Struct (mk_typename n)
     | None -> T.Typ.Int)
  | T_Int _
  | T_Bits _
  | T_Bool ->
    T.Typ.Int
  | _ ->
    T.Typ.Int

let lower_ty_annotated ty = mk_typ_annotated (lower_ty ty)

let field_typ struct_name field_name =
  match lookup_field_ty struct_name field_name with
  | Some ty -> lower_ty ty
  | None -> T.Typ.Int

let type_name_of_record_ty (fallback : string option) (ty : AST.ty) =
  match ty.desc with
  | T_Named n -> if named_type_is_struct n then Some n else fallback
  | T_Record fields | T_Exception fields ->
    find_struct_name_for_record fields
  | _ -> fallback

(* Lower ASL record/exception type declarations to Textual structs. *)
let lower_type_decl (name : string) (ty : AST.ty) : T.Module.decl option =
  match ty.desc with
  | T_Record fields | T_Exception fields ->
    let field_decls = List.map (fun (fname, fty) ->
      T.FieldDecl.{
        qualified_name = mk_fieldname name fname;
        typ = lower_ty fty;
        attributes = [];
      }
    ) fields in
    Some (T.Module.Struct T.Struct.{
      name = mk_typename name;
      supers = [];
      fields = field_decls;
      attributes = [];
    })
  | _ -> None

(* --- Expression lowering --- *)

(* Resolve the Textual struct type name for an ASL variable.

   Search order:
   1. local var_types map, populated by local declarations;
   2. current function arguments, using their ASL type annotations.
*)
let resolve_struct_name b name =
  match get_var_struct_type b name with
  | Some s -> Some s
  | None ->
    match List.assoc_opt name b.func_args with
    | Some ty ->
      (match ty.desc with
       | T_Named n -> if named_type_is_struct n then Some n else None
       | T_Record fields | T_Exception fields ->
         find_struct_name_for_record fields
       | _ -> None)
    | None -> None

(* Decide whether a structured ASL variable should be accessed as a value struct. *)
let is_value_struct_var b name =
  match resolve_struct_name b name with
  | None -> false
  | Some _ ->
    let vn = varname_of_ident name in
    List.exists (fun (v, _) -> T.VarName.equal v vn) b.locals
    ||
    match List.assoc_opt name b.func_args with
    | Some ty ->
      (match ty.desc with
       | T_Named _ | T_Record _ | T_Exception _ -> true
       | _ -> false)
    | None -> false

(* Peel away ASL ATC wrappers for field-access cases where the underlying value
   expression is what matters. This is not full ATC lowering. *)
let rec unwrap_expr (e : AST.expr) =
  match e.desc with
  | E_ATC (inner, _) -> unwrap_expr inner
  | _ -> e

let terminator_return_for_type (typ : T.Typ.t) (value : T.Exp.t option) =
  match typ, value with
  | T.Typ.Void, _ -> T.Terminator.Ret (T.Exp.Const T.Const.Null)
  | _, Some v -> T.Terminator.Ret v
  | _, None -> T.Terminator.Ret (T.Exp.Const (T.Const.Int Z.zero))

let terminate_return b value =
  terminate b (terminator_return_for_type b.result_type value)

let rec expr_record_struct_name b (e : AST.expr) : string option =
  let e = unwrap_expr e in
  match e.desc with
  | E_Var name -> resolve_struct_name b name
  | E_Record (ty, _) -> type_name_of_record_ty None ty
  | E_GetField (base, field) ->
    (match expr_record_struct_name b base with
     | Some base_struct ->
       (match lookup_field_ty base_struct field with
        | Some fty -> type_name_of_record_ty None fty
        | None -> None)
     | None -> None)
  | _ -> None

(* Materialize a computed ASL record value into Textual storage.

   This helper exists because Textual field
   access needs an addressable storage location.  It stores the value into a
   fresh struct-typed local and returns that local name. *)
let materialize_struct_value_to_lvar b struct_name value loc =
  let struct_typ = T.Typ.Struct (mk_typename struct_name) in
  let tmp = fresh_varname b "_record_base" in
  add_local_typed b tmp struct_typ;
  emit b (T.Instr.Store {
    exp1 = T.Exp.Lvar tmp;
    typ = Some struct_typ;
    exp2 = value;
    loc;
  });
  tmp

(* Load a field from an addressable record storage location.

   The base expression here is a local variable, whose Textual expression denotes
   the address of the local storage slot; it is Textual's syntax for accessing a 
   field of local storage. *)
let load_field_from_lvar b base_lvar struct_name field_name loc =
  let id = fresh_id b in
  emit b (T.Instr.Load {
    id;
    exp =
      T.Exp.Field {
        exp = T.Exp.Lvar base_lvar;
        field = mk_fieldname struct_name field_name;
      };
    typ = Some (field_typ struct_name field_name);
    loc;
  });
  T.Exp.Var id

(* Lower an ASL expression to a Textual expression.

   Convention: complex expressions usually emit instructions that define a fresh
   SSA id, then return T.Exp.Var id. Simple constants return a constant directly. *)
let local_type_of_var b vn =
  b.locals
  |> List.find_opt (fun (v, _) -> T.VarName.equal v vn)
  |> Option.map (fun (_, typ_annot) -> typ_annot.T.Typ.typ)

let rec expr_result_typ b e =
  match (unwrap_expr e).desc with
  | E_Var name ->
    let vn = varname_of_ident name in
    begin
      match local_type_of_var b vn with
      | Some typ -> typ
      | None -> T.Typ.Int
    end

  | E_Tuple es ->
    tuple_struct_typ (List.length es)

    (* Why is an E_Call of type tuple?*)
  | E_Call { name; _ } ->
    begin
      match Hashtbl.find_opt _func_return_tuple_arity name with
      | Some n -> tuple_struct_typ n
      | None -> T.Typ.Int
    end

  | E_Cond (_cond, e_then, _e_else) ->
    expr_result_typ b e_then

  | _ ->
    T.Typ.Int
  
(* Is it okay to replace type for all matches of the variable name? *)
let replace_local_type b vn typ =
  b.locals <-
    List.map
      (fun (v, old_typ) ->
        if T.VarName.equal v vn then
          (v, mk_typ_annotated typ)
        else
          (v, old_typ))
      b.locals

let ensure_array_local b name =
  let vn = varname_of_ident name in
  replace_local_type b vn (T.Typ.Array T.Typ.Int);
  add_var_type b name "__array"

let bool_const b =
  T.Exp.Const (T.Const.Int (Z.of_int (if b then 1 else 0)))

let emit_builtin_call b loc name args =
  let id = fresh_id b in
  emit b (T.Instr.Let {
    id = Some id;
    exp = T.Exp.call_non_virtual (mk_procname name) args;
    loc
  });
  T.Exp.Var id

let emit_call_ret = emit_builtin_call

let emit_eq b loc x y =
  emit_builtin_call b loc "__sil_eq" [x; y]

let emit_le b loc x y =
  emit_builtin_call b loc "__sil_le" [x; y]

let emit_lnot b loc x =
  emit_builtin_call b loc "__sil_lnot" [x]

let emit_land b loc x y =
  emit_builtin_call b loc "__sil_land" [x; y]

let emit_lor b loc x y =
  emit_builtin_call b loc "__sil_lor" [x; y]

let rec lower_expr b (e : AST.expr) : T.Exp.t =
  let loc = loc_of_pos e.pos_start in
  b.last_loc <- loc;
  let rec and_chain = function
    | [] -> bool_const true
    | [x] -> x
    | x :: xs ->
      let y = and_chain xs in
      emit_land b loc x y
  in
  let textual_typ_of_asl_ty ty =
  match ty.desc with
  | T_Tuple tys ->
    tuple_struct_typ (List.length tys)
  | T_Array _ ->
    T.Typ.Array T.Typ.Int
  (* What about records and exceptions? Does this branch include E_Call? Why?*)
  | T_Named name ->
    begin
      match name with
      | s when String.length s >= 8 && String.sub s 0 8 = "__tuple_" ->
        T.Typ.Struct (mk_typename s)
      | _ ->
        T.Typ.Int
    end
  | _ ->
    T.Typ.Int
  in
  let formal_type_of_var b name =
    let key = name in
    b.func_args
    |> List.find_opt (fun (arg_name, _) -> String.equal arg_name key)
    |> Option.map (fun (_, ty) -> textual_typ_of_asl_ty ty)
  in
  let tuple_arity_of_var b name =
    let key = name in
    match List.assoc_opt key b.var_types with
    | Some struct_name when String.starts_with ~prefix:"__tuple_" struct_name ->
      begin
        try
          Some (int_of_string (String.sub struct_name 8 (String.length struct_name - 8)))
        with _ ->
          None
      end
    | _ ->
      begin
        match List.find_opt (fun (arg_name, _) -> String.equal arg_name key) b.func_args with
        | Some (_, { desc = T_Tuple tys; _ }) -> Some (List.length tys)
        | _ -> None
      end
    in
  let rec lower_pattern_match scrutinee pat =
    match pat.desc with
    | Pattern_All ->
      bool_const true

    | Pattern_Single e_pat ->
      let rhs = lower_expr b e_pat in
      emit_eq b loc scrutinee rhs

    | Pattern_Any pats ->
      begin
        match pats with
        | [] -> bool_const false
        | p :: ps ->
          List.fold_left
            (fun acc p ->
              let rhs = lower_pattern_match scrutinee p in
              emit_lor b loc acc rhs)
            (lower_pattern_match scrutinee p)
            ps
      end

    | Pattern_Not p ->
      let v = lower_pattern_match scrutinee p in
      emit_lnot b loc v

    | Pattern_Geq e_pat ->
      let rhs = lower_expr b e_pat in
      emit_le b loc rhs scrutinee

    | Pattern_Leq e_pat ->
      let rhs = lower_expr b e_pat in
      emit_le b loc scrutinee rhs

    | Pattern_Range (lo, hi) ->
      let lo_v = lower_expr b lo in
      let hi_v = lower_expr b hi in
      let ge_lo = emit_le b loc lo_v scrutinee in
      let le_hi = emit_le b loc scrutinee hi_v in
      emit_land b loc ge_lo le_hi

    | Pattern_Mask _ ->
      unsupported_expr "E_Pattern" e "mask patterns not implemented yet"

    | Pattern_Tuple pats ->
      let n = List.length pats in
      Hashtbl.replace _emitted_tuple_types n true;
      let tmp = materialize_tuple_value_to_lvar b n scrutinee loc in
      pats
      |> List.mapi (fun i p ->
          let field_v = load_tuple_field_from_lvar b tmp n i loc in
          lower_pattern_match field_v p)
      |> and_chain
      in
    let record_type_name_of_asl_ty ty =
    match ty.desc with
    | T_Named n when named_type_is_struct n ->
      Some n
    | T_Record fields | T_Exception fields ->
      find_struct_name_for_record fields
    | _ ->
      None
    in
    let struct_name_of_var b name =
    let key = name in
    match List.assoc_opt key b.var_types with
    | Some s ->
      Some s
    | None ->
      begin
        match
          List.find_opt
            (fun (arg_name, _) -> String.equal arg_name key)
            b.func_args
        with
        | Some (_, ty) ->
          record_type_name_of_asl_ty ty
        | None ->
          None
      end
    in
  match e.desc with
  (* Primitive literals. Strings and reals are not yet handled precisely. *)
  | E_Literal (L_Int z) ->
    T.Exp.Const (T.Const.Int z)
  | E_Literal (L_Bool true) ->
    T.Exp.Const (T.Const.Int Z.one)
  | E_Literal (L_Bool false) ->
    T.Exp.Const (T.Const.Int Z.zero)
  | E_Literal (L_BitVector bv) ->
    T.Exp.Const (T.Const.Int (Z.of_int (Bitvector.to_int bv)))
  | E_Literal (L_Label s) ->
    T.Exp.Const (T.Const.Int (Z.of_int (Hashtbl.hash s)))
  | E_Literal (L_String _) ->
      T.Exp.Const (T.Const.Int Z.zero)

  | E_Literal (L_Real _) ->
      T.Exp.Const (T.Const.Int Z.zero)


  | E_ATC (inner, ty) ->
    (* ASL ATC ("as type") is a runtime check/refinement operation.  The scalar
       cases below lower the value and, where the target is a constrained integer,
       emit Textual Prune instructions for the known constraints.  Width-aware
       bitvector normalization is delegated to later bitvector helpers. *)
    let v = lower_expr b inner in
    (match ty.desc with
    (* Should we do this where we currently approximate constrained Ints as well? *)
     | T_Int (WellConstrained (constraints, _)) ->
       let constraint_exps = List.filter_map (fun c ->
         match c with
         | Constraint_Exact ce ->
           let ce_v = lower_expr b ce in
           let cid = fresh_id b in
           emit b (T.Instr.Let {
             id = Some cid;
             exp = T.Exp.call_non_virtual
               (T.ProcDecl.of_binop IR.Binop.Eq) [v; ce_v];
             loc });
           Some (T.Exp.Var cid)
         | Constraint_Range (lo, hi) ->
           let lo_v = lower_expr b lo in
           let hi_v = lower_expr b hi in
           let ge_id = fresh_id b in
           emit b (T.Instr.Let {
             id = Some ge_id;
             exp = T.Exp.call_non_virtual
               (T.ProcDecl.of_binop IR.Binop.Ge) [v; lo_v];
             loc });
           let le_id = fresh_id b in
           emit b (T.Instr.Let {
             id = Some le_id;
             exp = T.Exp.call_non_virtual
               (T.ProcDecl.of_binop IR.Binop.Le) [v; hi_v];
             loc });
           let and_id = fresh_id b in
           emit b (T.Instr.Let {
             id = Some and_id;
             exp = T.Exp.call_non_virtual
               (T.ProcDecl.of_binop IR.Binop.LAnd)
               [T.Exp.Var ge_id; T.Exp.Var le_id];
             loc });
           Some (T.Exp.Var and_id)
       ) constraints in
       (match constraint_exps with
        | [] -> ()
        | first :: rest ->
          let combined = List.fold_left (fun acc ce ->
            let or_id = fresh_id b in
            emit b (T.Instr.Let {
              id = Some or_id;
              exp = T.Exp.call_non_virtual
                (T.ProcDecl.of_binop IR.Binop.LOr) [acc; ce];
              loc });
            T.Exp.Var or_id
          ) first rest in
          emit b (T.Instr.Prune { exp = combined; loc }))
     | T_Int _
     | T_Bits _
     | T_Bool
     | T_Named _ ->
       ()
     | _ ->
       unsupported_expr "E_ATC" e "unsupported cast target");
    v

  (* Variable read. The local/argument type is used to choose the Textual load
     type. For record values this should be Struct T. *)
    | E_Var name ->
      let vn = varname_of_ident name in

      let is_local =
        List.exists (fun (v, _) -> T.VarName.equal v vn) b.locals
      in
      let is_formal =
        List.exists
          (fun (arg_name, _) -> String.equal arg_name name)
          b.func_args
      in

      if
        not is_local
        && not is_formal
        && not (Hashtbl.mem _defined_globals name)
      then
        Hashtbl.replace _external_globals name ();

      let typ =
        match local_type_of_var b vn with
        | Some typ -> typ
        | None ->
          begin
            match formal_type_of_var b name with
            | Some typ -> typ
            | None -> T.Typ.Int
          end
      in

      let id = fresh_id b in
      emit b (T.Instr.Load {
        id;
        exp = T.Exp.Lvar vn;
        typ = Some typ;
        loc;
      });
      T.Exp.Var id

  (* Boolean equality. *)
  | E_Binop (`BEQ, lhs, rhs) ->
    let l = lower_expr b lhs in
    let r = lower_expr b rhs in
    let eq = emit_eq b loc l r in
    eq

  (* Operators below are preserved as named opaque helpers. 
     This frontend does not encode their full semantics yet. *)
  | E_Binop (`RDIV, lhs, rhs) ->
    _needs_rdiv_helper := true;
    let l = lower_expr b lhs in
    let r = lower_expr b rhs in
    emit_call_ret b loc "__asl_rdiv" [l; r]

  | E_Binop (`POW, lhs, rhs) ->
    _needs_pow_helper := true;
    let l = lower_expr b lhs in
    let r = lower_expr b rhs in
    emit_call_ret b loc "__asl_pow" [l; r]

  | E_Binop (`DIVRM, lhs, rhs) ->
    _needs_divrm_helper := true;
    let l = lower_expr b lhs in
    let r = lower_expr b rhs in
    emit_call_ret b loc "__asl_divrm" [l; r]

  | E_Binop (`STR_CONCAT, lhs, rhs) ->
    _needs_str_concat_helper := true;
    let l = lower_expr b lhs in
    let r = lower_expr b rhs in
    emit_call_ret b loc "__asl_str_concat" [l; r]

  | E_Binop (`BV_CONCAT, lhs, rhs) ->
    _needs_bv_concat_helper := true;
    let l = lower_expr b lhs in
    let r = lower_expr b rhs in
    emit_call_ret b loc "__asl_bv_concat" [l; r]

  (* Low-fidelity: ASL boolean implication is represented by an opaque helper.
     This preserves the operation name but does not encode short-circuit control
     flow in Textual. Prefer explicit CFG lowering when implemented. *)
  | E_Binop (`IMPL, lhs, rhs) ->
    _needs_bool_impl_helper := true;
    let l = lower_expr b lhs in
    let r = lower_expr b rhs in
    emit_call_ret b loc "ASL_BoolImpl" [l; r]

  (* Binary and unary operations lower through Textual builtin operator procs. *)
  | E_Binop (op, lhs, rhs) ->
    let l = lower_expr b lhs in
    let r = lower_expr b rhs in
    let id = fresh_id b in
    emit b (T.Instr.Let {
      id = Some id;
      exp = T.Exp.call_non_virtual (sil_of_binop op) [l; r];
      loc });
    T.Exp.Var id

  (* Literal integer unary minus. *)
  | E_Unop (NEG, ({ desc = E_Literal lit; _ } as e1)) ->
  begin
    match lit with
    | L_Int z ->
      T.Exp.Const (T.Const.Int (Z.neg z))

    | _ ->
      unsupported_expr "E_Unop" e
        "non-integer literal unary minus not verifier-clean yet: %s"
        (string_of_expr_desc e1)
  end

  (* TODO: Non-literal integer unary minus lowers as 0 - value using the same
     Textual arithmetic operator path as binary subtraction. *)
  | E_Unop (NEG, e1) ->
    let zero = T.Exp.Const (T.Const.Int Z.zero) in
    let v = lower_expr b e1 in
    let id = fresh_id b in
    emit b (T.Instr.Let {
      id = Some id;
      exp = T.Exp.call_non_virtual (sil_of_binop `SUB) [zero; v];
      loc
    });
    T.Exp.Var id

  | E_Unop (op, e1) ->
    let v = lower_expr b e1 in
    let id = fresh_id b in
    emit b (T.Instr.Let {
      id = Some id;
      exp = T.Exp.call_non_virtual (sil_of_unop op) [v];
      loc
    });
    T.Exp.Var id

  (* Currently quite adhoc - for e.g., ROL/ROR support *)
  | E_Call { name; args; _ } ->
    let arg_vals = List.map (lower_expr b) args in
    let arity = List.length args in

    if not (Hashtbl.mem _defined_procs (name, arity)) then
      Hashtbl.replace _external_calls (name, arity) ();

    begin
      match !_expected_call_return_tuple_arity with
      | Some n ->
        Hashtbl.replace _external_return_tuple_arity (name, arity) n
      | None ->
        begin
          match Hashtbl.find_opt _func_return_tuple_arity name with
          | Some n ->
            Hashtbl.replace _external_return_tuple_arity (name, arity) n
          | None ->
            begin
              match name, arity with
              | ("ROL_C" | "ROR_C"), 2 ->
                Hashtbl.replace _external_return_tuple_arity (name, arity) 2
              | _ ->
                ()
            end
        end
    end;

    let id = fresh_id b in
    emit b (T.Instr.Let {
      id = Some id;
      exp = T.Exp.call_non_virtual (mk_procname name) arg_vals;
      loc;
    });
    T.Exp.Var id

  (* Ternary expression is lowered to blocks and a synthetic local. Both branches
     store into the same temporary, then the merge block loads it. *)
  | E_Cond (cond, e_then, e_else) ->
    let cond_val = lower_expr b cond in

    let result_typ = expr_result_typ b e_then in
    let result_vn = fresh_varname b "_cond" in
    add_local_typed b result_vn result_typ;

    let then_lbl = fresh_label b "tern_then" in
    let else_lbl = fresh_label b "tern_else" in
    let merge_lbl = fresh_label b "tern_merge" in

    terminate b (mk_jump [then_lbl; else_lbl]);

    start_block b then_lbl;
    emit b (T.Instr.Prune { exp = cond_val; loc });
    let tv = lower_expr b e_then in
    emit b (T.Instr.Store {
      exp1 = T.Exp.Lvar result_vn;
      typ = Some result_typ;
      exp2 = tv;
      loc
    });
    terminate b (mk_jump [merge_lbl]);

    start_block b else_lbl;
    emit b (T.Instr.Prune { exp = T.Exp.not cond_val; loc });
    let fv = lower_expr b e_else in
    emit b (T.Instr.Store {
      exp1 = T.Exp.Lvar result_vn;
      typ = Some result_typ;
      exp2 = fv;
      loc
    });
    terminate b (mk_jump [merge_lbl]);

    start_block b merge_lbl;
    let id = fresh_id b in
    emit b (T.Instr.Load {
      id;
      exp = T.Exp.Lvar result_vn;
      typ = Some result_typ;
      loc
    });
    T.Exp.Var id

  (* Record field read.

     If the base is an addressable local, read the field directly from that
     local's storage slot.

     If the base is a computed record value, first materialize that value into a
     fresh struct-typed local, then read the field from that temporary storage.
     This satisfies Textual's requirement that field accesses operate on 
     addressable storage. *)
  | E_GetField (e_base, field_name) ->
    let e_base_unwrapped = unwrap_expr e_base in
    let struct_name =
      match expr_record_struct_name b e_base with
      | Some s ->
        s

      | None ->
        begin
          match e_base_unwrapped.desc with
          | E_Var base_name ->
            begin
              match struct_name_of_var b base_name with
              | Some s ->
                s
              | None ->
                unsupported_expr "E_GetField" e
                  "could not resolve struct type for base expression %s.%s"
                  (string_of_expr_desc e_base_unwrapped)
                  field_name
            end

          | _ ->
            unsupported_expr "E_GetField" e
              "could not resolve struct type for base expression %s.%s"
              (string_of_expr_desc e_base_unwrapped)
              field_name
        end
    in
    begin
      match e_base_unwrapped.desc with
      | E_Var base_name ->
        load_field_from_lvar
          b
          (varname_of_ident base_name)
          struct_name
          field_name
          loc

      | _ ->
        let base_v = lower_expr b e_base in
        let tmp =
          materialize_struct_value_to_lvar b struct_name base_v loc
        in
        load_field_from_lvar b tmp struct_name field_name loc
    end

  (* ARBITRARY integer: model as a call to an opaque helper plus optional
     constraint prunes. *)
  | E_Arbitrary ty ->
    _needs_arbitrary := true;
    let id = fresh_id b in
    emit b (T.Instr.Let {
      id = Some id;
      exp = T.Exp.call_non_virtual (mk_procname "ASL_Arbitrary") [];
      loc });
    (match ty.desc with
     | T_Int (WellConstrained (constraints, _)) ->
       let val_exp = T.Exp.Var id in
       let constraint_exps = List.filter_map (fun c ->
         match c with
         | Constraint_Exact e ->
           let ce = lower_expr b e in
           let cid = fresh_id b in
           emit b (T.Instr.Let {
             id = Some cid;
             exp = T.Exp.call_non_virtual
               (T.ProcDecl.of_binop IR.Binop.Eq) [val_exp; ce];
             loc });
           Some (T.Exp.Var cid)
         | Constraint_Range (lo, hi) ->
           let lo_e = lower_expr b lo in
           let hi_e = lower_expr b hi in
           let ge_id = fresh_id b in
           emit b (T.Instr.Let {
             id = Some ge_id;
             exp = T.Exp.call_non_virtual
               (T.ProcDecl.of_binop IR.Binop.Ge) [val_exp; lo_e];
             loc });
           let le_id = fresh_id b in
           emit b (T.Instr.Let {
             id = Some le_id;
             exp = T.Exp.call_non_virtual
               (T.ProcDecl.of_binop IR.Binop.Le) [val_exp; hi_e];
             loc });
           let and_id = fresh_id b in
           emit b (T.Instr.Let {
             id = Some and_id;
             exp = T.Exp.call_non_virtual
               (T.ProcDecl.of_binop IR.Binop.LAnd)
               [T.Exp.Var ge_id; T.Exp.Var le_id];
             loc });
           Some (T.Exp.Var and_id)
       ) constraints in
       if constraint_exps <> [] then begin
         let combined = List.fold_left (fun acc e ->
           let or_id = fresh_id b in
           emit b (T.Instr.Let {
             id = Some or_id;
             exp = T.Exp.call_non_virtual
               (T.ProcDecl.of_binop IR.Binop.LOr) [acc; e];
             loc });
           T.Exp.Var or_id
         ) (List.hd constraint_exps) (List.tl constraint_exps) in
         emit b (T.Instr.Prune { exp = combined; loc })
       end
     | _ -> ());
    T.Exp.Var id

  | E_Pattern (e_scrutinee, pat) ->
    let scrutinee = lower_expr b e_scrutinee in
    lower_pattern_match scrutinee pat
  
  (* Record expression.

     Create temporary value storage, initialize each field, then load 
     the whole struct value from that storage.

     *)
  | E_Record (ty, fields) ->
    let type_name =
      match type_name_of_record_ty None ty with
      | Some n -> n
      | None ->
        unsupported_expr "E_Record" e
          "anonymous record literal could not be matched to a declared struct"
    in
    let struct_typ = T.Typ.Struct (mk_typename type_name) in
    let tmp = fresh_varname b "_record" in
    add_local_typed b tmp struct_typ;

    List.iter
      (fun (field_name, field_expr) ->
        let v = lower_expr b field_expr in
        emit b (T.Instr.Store {
          exp1 =
            T.Exp.Field {
              exp = T.Exp.Lvar tmp;
              field = mk_fieldname type_name field_name;
            };
          typ = Some (field_typ type_name field_name);
          exp2 = v;
          loc;
        }))
      fields;

    let id = fresh_id b in
    emit b (T.Instr.Load {
      id;
      exp = T.Exp.Lvar tmp;
      typ = Some struct_typ;
      loc;
    });
    T.Exp.Var id
  
  (* Bitvector slice expression.

     Single slices are emitted as named opaque helpers carrying the slice kind
     and operands.  Multi-slice lowering is coarser: ASL_BVSliceMulti keeps
     only the base value.  These are placeholders. *)
  | E_Slice (base, slices) ->
    let base_v = lower_expr b base in
    let lower_one_slice = function
      | Slice_Length (start, width) ->
        _needs_bv_slice_length_helper := true;
        let start_v = lower_expr b start in
        let width_v = lower_expr b width in
        emit_call_ret b loc "ASL_BVSliceLength" [base_v; start_v; width_v]

      | Slice_Range (lo, hi) ->
        _needs_bv_slice_range_helper := true;
        let lo_v = lower_expr b lo in
        let hi_v = lower_expr b hi in
        emit_call_ret b loc "ASL_BVSliceRange" [base_v; lo_v; hi_v]

      | Slice_Single index ->
        _needs_bv_slice_single_helper := true;
        let index_v = lower_expr b index in
        emit_call_ret b loc "ASL_BVSliceSingle" [base_v; index_v]

      | _ ->
        unsupported_expr "E_Slice" e
          "slice form not implemented yet"
    in
    begin
    match slices with
    | [slice] ->
      lower_one_slice slice
    | _ :: _ :: _ ->
      _needs_bv_slice_multi_helper := true;
      emit_call_ret b loc "ASL_BVSliceMulti" [base_v]
    | [] ->
      unsupported_expr "E_Slice" e
        "empty slice list not implemented yet"
    end

  (* Tuple expression.

     Construct the tuple in temporary value storage,
     then load the whole tuple value as the result of the expression.

     *)
  | E_Tuple exprs ->
    let n = List.length exprs in
    let tmp = fresh_varname b "_tuple" in
    add_tuple_local b tmp n;
    Hashtbl.replace _emitted_tuple_types n true;

    List.iteri
      (fun i item_expr ->
        let v = lower_expr b item_expr in
        emit b (T.Instr.Store {
          exp1 =
            T.Exp.Field {
              exp = T.Exp.Lvar tmp;
              field = tuple_fieldname n i;
            };
          typ = Some T.Typ.Int;
          exp2 = v;
          loc;
        }))
      exprs;

    let id = fresh_id b in
    emit b (T.Instr.Load {
      id;
      exp = T.Exp.Lvar tmp;
      typ = Some (tuple_struct_typ n);
      loc;
    });
    T.Exp.Var id

  (* Array read. Currently only simple variable bases are accepted. *)
  | E_GetArray (e_base, e_index) ->
    let vn =
      match e_base.desc with
      | E_Var v -> v
      | _ ->
        unsupported_expr "E_GetArray" e
          "non-variable array base %s"
          (string_of_expr_desc e_base)
    in
    ensure_array_local b vn;

    let index = lower_expr b e_index in
    let id = fresh_id b in
    emit b (T.Instr.Load {
      id;
      exp = T.Exp.Index (T.Exp.Lvar (varname_of_ident vn), index);
      typ = Some T.Typ.Int;
      loc
    });
    T.Exp.Var id

  (* Tuple item read.

     For a tuple local, read directly from the local storage slot.  For a
     computed tuple value, first copy the value into temporary tuple storage,
     then read the requested field.

     Note: Tuple extraction is exact only when the tuple arity can
     be recovered from a literal tuple, local metadata, formal type, or helper
     struct name.  Otherwise it remains unsupported. *)
  | E_GetItem (base, index) ->
    let base_unwrapped = unwrap_expr base in

    let tuple_arity =
      match base_unwrapped.desc with
      | E_Tuple exprs ->
        Some (List.length exprs)

      | E_Var name ->
        begin
          match tuple_arity_of_var b name with
          | Some n -> Some n
          | None ->
            begin
              match resolve_struct_name b name with
              | Some s when String.length s >= 8
                        && String.sub s 0 8 = "__tuple_" ->
                begin
                  try
                    Some
                      (int_of_string
                         (String.sub s 8 (String.length s - 8)))
                  with Failure _ ->
                    None
                end
              | _ ->
                None
            end
        end

      | _ ->
        None
    in

    let n =
      match tuple_arity with
      | Some n -> n
      | None ->
        unsupported_expr "E_GetItem" e
          "could not resolve tuple type for base expression %s"
          (string_of_expr_desc base_unwrapped)
    in

    if index < 0 || index >= n then
      unsupported_expr "E_GetItem" e
        "tuple index %d out of range for tuple arity %d" index n;

    Hashtbl.replace _emitted_tuple_types n true;

    begin
      match base_unwrapped.desc with
      | E_Var name ->
        load_tuple_field_from_lvar
          b
          (varname_of_ident name)
          n
          index
          loc

      | _ ->
        let value = lower_expr b base_unwrapped in
        let tmp = materialize_tuple_value_to_lvar b n value loc in
        load_tuple_field_from_lvar b tmp n index loc
    end

  (* Deliberate fallback: do not silently approximate unsupported expressions. *)
  | _ ->
    unsupported_expr (string_of_expr_desc e) e "%a" Asllib.PP.pp_expr e

(* Materialize an exception expression into addressable storage for Textual
   throw.

   This helper is for Textual's exception
   ABI: the throw terminator expects a pointer/address to the thrown object. *)

let rec lower_exception_expr_to_lvar b (e : AST.expr) loc : T.VarName.t =
  let e_unwrapped = unwrap_expr e in
  match e_unwrapped.desc with
  | E_Record (ty, fields) ->
    let type_name =
      match type_name_of_record_ty None ty with
      | Some n -> n
      | None ->
        unsupported_expr "S_Throw" e
          "anonymous exception literal could not be matched to a declared struct"
    in
    let struct_typ = T.Typ.Struct (mk_typename type_name) in
    let tmp = fresh_varname b "_exn" in
    add_local_typed b tmp struct_typ;

    List.iter
      (fun (field_name, field_expr) ->
        let v = lower_expr b field_expr in
        emit b (T.Instr.Store {
          exp1 =
            T.Exp.Field {
              exp = T.Exp.Lvar tmp;
              field = mk_fieldname type_name field_name;
            };
          typ = Some (field_typ type_name field_name);
          exp2 = v;
          loc;
        }))
      fields;

    tmp

  | E_Var name ->
    (* Throwing an already stored exception local.  Textual throw wants the
       address of that local storage. *)
    varname_of_ident name

  | _ ->
    (* Computed exception value.  Lower to a value, then copy into fresh
       addressable storage before throwing. *)
    let struct_name =
      match expr_record_struct_name b e_unwrapped with
      | Some s -> s
      | None ->
        unsupported_expr "S_Throw" e
          "could not resolve exception struct type for thrown expression %s"
          (string_of_expr_desc e_unwrapped)
    in
    let value = lower_expr b e_unwrapped in
    materialize_struct_value_to_lvar b struct_name value loc

(* Store a previously lowered RHS value into an ASL left-hand side.

   The RHS is evaluated once by S_Assign before calling this function,
   preserving ASL evaluation order. *)

and lower_store_lexpr ?rhs_expr b (le : AST.lexpr) (v : T.Exp.t) loc =

  let lower_slice_update name helper_flag helper_name slice_args =
    helper_flag := true;
    let vn = varname_of_ident name in

    let old_id = fresh_id b in
    emit b (T.Instr.Load {
      id = old_id;
      exp = T.Exp.Lvar vn;
      typ = Some T.Typ.Int;
      loc;
    });
    let old_v = T.Exp.Var old_id in

    let arg_vs = List.map (lower_expr b) slice_args in
    let new_v =
      emit_call_ret b loc helper_name (old_v :: (arg_vs @ [v]))
    in

    emit b (T.Instr.Store {
      exp1 = T.Exp.Lvar vn;
      typ = Some T.Typ.Int;
      exp2 = new_v;
      loc;
    })
  in
  match le.desc with
  (* Simple variable assignment. Current default type is int. *)
  | LE_Var id ->
    let vn = varname_of_ident id in
    let typ =
      match List.find_opt (fun (v0, _) -> T.VarName.equal v0 vn) b.locals with
      | Some (_, annot) -> annot.typ
      | None -> T.Typ.Int
    in
    emit b (T.Instr.Store {
      exp1 = T.Exp.Lvar vn;
      typ = Some typ;
      exp2 = v;
      loc
    })

  (* Record field assignment.

     Direct field assignment is supported only when the base is an addressable
     ASL local. This updates the field of that local's value storage.

     Nested field assignment needs read-modify-write to preserve ASL value
     semantics. *)
  | LE_SetField (le_base, field_name) ->
    begin
      match le_base.desc with
      | LE_Var base_name ->
        let struct_name =
          match resolve_struct_name b base_name with
          | Some s -> s
          | None ->
            unsupported_lexpr "LE_SetField" le
              "could not resolve struct type for base variable %s.%s"
              base_name field_name
        in
        emit b (T.Instr.Store {
          exp1 =
            T.Exp.Field {
              exp = T.Exp.Lvar (varname_of_ident base_name);
              field = mk_fieldname struct_name field_name;
            };
          typ = Some (field_typ struct_name field_name);
          exp2 = v;
          loc;
        })

      | _ ->
        unsupported_lexpr "LE_SetField" le
          "non-variable field base %s requires read-modify-write lowering"
          (string_of_lexpr_desc le_base)
    end

  (* Array element assignment. Only simple variable array bases are supported. *)
  | LE_SetArray (le_base, e_index) ->
    let vn =
      match le_base.desc with
      | LE_Var v -> v
      | _ ->
        unsupported_lexpr "LE_SetArray" le
          "non-variable array base %s"
          (string_of_lexpr_desc le_base)
    in
    ensure_array_local b vn;

    let index = lower_expr b e_index in
    emit b (T.Instr.Store {
      exp1 = T.Exp.Index (T.Exp.Lvar (varname_of_ident vn), index);
      typ = Some T.Typ.Int;
      exp2 = v;
      loc
    })
  | LE_SetCollectionFields (_name, _fields, _ty_annot) ->
    unsupported_lexpr "LE_SetCollectionFields" le
      "collection-field update not implemented yet"

  (* Bitvector slice assignment.

     Simple variable-base updates are emitted as named opaque update helpers.
     They preserve the read/update/write shape but do not encode bit-level update
     semantics here.  Complex bases remain unsupported until read-modify-
     write lowering is implemented. *)
  | LE_Slice (le_base, slices) ->
    begin
      match le_base.desc, slices with
      | LE_Var name, [Slice_Length (start, width)] ->
        lower_slice_update
          name
          _needs_bv_update_length_helper
          "ASL_BVUpdateLength"
          [start; width]

      | LE_Var name, [Slice_Range (lo, hi)] ->
        lower_slice_update
          name
          _needs_bv_update_range_helper
          "ASL_BVUpdateRange"
          [lo; hi]

      | LE_Var name, [Slice_Single index] ->
        lower_slice_update
          name
          _needs_bv_update_single_helper
          "ASL_BVUpdateSingle"
          [index]

      | LE_Var name, _ :: _ :: _ ->
        _needs_bv_update_multi_helper := true;
        let vn = varname_of_ident name in

        let old_id = fresh_id b in
        emit b (T.Instr.Load {
          id = old_id;
          exp = T.Exp.Lvar vn;
          typ = Some T.Typ.Int;
          loc;
        });

        let new_v =
          emit_call_ret b loc "ASL_BVUpdateMulti"
            [T.Exp.Var old_id; v]
        in

        emit b (T.Instr.Store {
          exp1 = T.Exp.Lvar vn;
          typ = Some T.Typ.Int;
          exp2 = new_v;
          loc;
        })

      | _ ->
        unsupported_lexpr "LE_Slice" le
          "slice assignment form not implemented yet"
    end

  (* Discard assignment. RHS was already evaluated before this helper was called,
     so side effects are preserved and the value is ignored. *)
  | LE_Discard ->
    ()

  (* Tuple destructuring assignment.

     Example:
       (a, b) = Foo();

     Lowering:
       tmp = Foo();        -- done before this helper
       t0 = tmp.item0; a = t0;
       t1 = tmp.item1; b = t1;

     Nested destructuring works by recursively calling lower_store_lexpr. *)
  | LE_Destructuring les ->
    let n = List.length les in
    Hashtbl.replace _emitted_tuple_types n true;

    begin
      match rhs_expr with
      | Some rhs ->
        begin
          match (unwrap_expr rhs).desc with
          | E_Call { name; args; _ } ->
            let arity = List.length args in
            if not (Hashtbl.mem _defined_procs (name, arity)) then begin
              Hashtbl.replace _external_calls (name, arity) ();
              Hashtbl.replace _external_return_tuple_arity (name, arity) n
            end
          | _ -> ()
        end
      | None -> ()
    end;

  let tmp = materialize_tuple_value_to_lvar b n v loc in

  List.iteri
    (fun i sub_le ->
      let item = load_tuple_field_from_lvar b tmp n i loc in
      lower_store_lexpr b sub_le item loc)
    les

  (* Fallback for harder l-expressions such as slices, enum arrays,
     collection fields, and multi-field updates. *)
  | _ ->
    unsupported_lexpr (string_of_lexpr_desc le) le ""

(* --- Statement lowering --- *)

(* Lower a statement by appending instructions/nodes to the builder. If the
   current block is already terminated, further statements are ignored as dead. *)
and lower_stmt b (s : AST.stmt) =
  if b.terminated then ()
  else
    let loc = loc_of_pos s.pos_start in
    b.last_loc <- loc;
    match s.desc with
    (* No-op. *)
    | S_Pass -> ()

    (* Sequential composition. *)
    | S_Seq (s1, s2) ->
      lower_stmt b s1;
      lower_stmt b s2

    (* Return with value. For struct variables, return the value lvar directly;
      otherwise return the lowered expression value. *)
    | S_Return (Some e) ->
      let e_unwrapped = unwrap_expr e in
      let v =
        match e_unwrapped.desc with
        | E_Var name when is_value_struct_var b name ->
          let vn = varname_of_ident name in
          let typ =
            match local_type_of_var b vn with
            | Some typ -> typ
            | None -> T.Typ.Int
          in
          let id = fresh_id b in
          emit b (T.Instr.Load {
            id;
            exp = T.Exp.Lvar vn;
            typ = Some typ;
            loc;
          });
          T.Exp.Var id

        | _ ->
          lower_expr b e
      in
      terminate_return b (Some v)

    (* Void ASL return must emit a void return. *)
    | S_Return None ->
      terminate_return b None

    (* Variable declaration. This case handles normal scalar declarations, arrays,
      named record/exception values, and tuple-typed value locals. *)
    | S_Decl (_, LDI_Var name, ty_opt, init) ->
      (match ty_opt with

      (* Tuple-typed local *)
      | Some ty when (match ty.desc with T_Tuple _ -> true | _ -> false) ->
        let n = (match ty.desc with T_Tuple ts -> List.length ts | _ -> 2) in
        let vn = varname_of_ident name in
        let tuple_typ = tuple_struct_typ n in

        Hashtbl.replace _emitted_tuple_types n true;
        add_local_typed b vn tuple_typ;
        add_var_type b name (tuple_struct_name n);

        (match init with
          | Some e ->
            let v = lower_expr b e in
            emit b (T.Instr.Store {
              exp1 = T.Exp.Lvar vn;
              typ = Some tuple_typ;
              exp2 = v;
              loc })
          | None -> ())

      (* Array local. Element type is currently approximated as int. *)
      | Some ty when (match ty.desc with T_Array _ -> true | _ -> false) ->
        let vn = varname_of_ident name in
        let arr_typ = lower_ty ty in
        if not (List.exists (fun (v, _) -> T.VarName.equal v vn) b.locals) then
          b.locals <- b.locals @ [(vn, mk_typ_annotated arr_typ)];
        add_var_type b name "__array"

      (* Named records/exceptions. *)
      | Some ty when
        (match ty.desc with
          | T_Named n -> named_type_is_struct n
          | T_Record _ | T_Exception _ -> true
          | _ -> false) ->
        let type_name =
          match ty.desc with
          | T_Named n -> n
          | T_Record fields | T_Exception fields ->
            (match find_struct_name_for_record fields with
              | Some n -> n
              | None -> name ^ "_record")
          | _ -> name ^ "_record"
        in
        let vn = varname_of_ident name in
        let struct_typ = T.Typ.Struct (mk_typename type_name) in
        if not (List.exists (fun (v, _) -> T.VarName.equal v vn) b.locals) then
          b.locals <- b.locals @ [(vn, mk_typ_annotated struct_typ)];
        add_var_type b name type_name;
        (match init with
          | Some e ->
            begin
              match (unwrap_expr e).desc with
              | E_Tuple es ->
                let n = List.length es in
                let vn = varname_of_ident name in
                let tuple_typ = tuple_struct_typ n in

                Hashtbl.replace _emitted_tuple_types n true;
                add_local_typed b vn tuple_typ;
                add_var_type b name (tuple_struct_name n);

                let v = lower_expr b e in
                emit b (T.Instr.Store {
                  exp1 = T.Exp.Lvar vn;
                  typ = Some tuple_typ;
                  exp2 = v;
                  loc
                })

              | E_Record (record_ty, _) ->
                let type_name =
                  match record_ty.desc with
                  | T_Named n -> n
                  | T_Record fields | T_Exception fields ->
                    begin
                      match find_struct_name_for_record fields with
                      | Some n -> n
                      | None -> name ^ "_record"
                    end
                  | _ -> type_name
                in
                add_var_type b name type_name;

                let v = lower_expr b e in
                emit b (T.Instr.Store {
                  exp1 = T.Exp.Lvar vn;
                  typ = Some struct_typ;
                  exp2 = v;
                  loc
                })

              | _ ->
                let v = lower_expr b e in
                emit b (T.Instr.Store {
                  exp1 = T.Exp.Lvar vn;
                  typ = Some struct_typ;
                  exp2 = v;
                  loc
                })
            end
          | None -> ())

      (* Default declaration path. If the initializer itself is a record or tuple
        literal, preserve structured value metadata; otherwise declare an int local. *)
      | _ ->
        let is_record_init = match init with
          | Some e -> (match e.desc with E_Record _ -> true | _ -> false)
          | None -> false
        in
        if is_record_init then begin
          let e = (match init with Some e -> e | None -> assert false) in
          let type_name =
            match e.desc with
            | E_Record (record_ty, _) ->
              (match type_name_of_record_ty (Some (name ^ "_record")) record_ty with
                | Some n -> n
                | None -> name ^ "_record")
            | _ -> assert false
          in
          let vn = varname_of_ident name in
          let struct_typ = T.Typ.Struct (mk_typename type_name) in
          add_local_typed b vn struct_typ;
          add_var_type b name type_name;
          let v = lower_expr b e in
          emit b (T.Instr.Store {
            exp1 = T.Exp.Lvar vn;
            typ = Some struct_typ;
            exp2 = v;
            loc })
        end else
        let structured_init_typ =
          match init with
          | Some e ->
            let typ = expr_result_typ b e in
            begin
              match typ with
              | T.Typ.Int ->
                None
              | _ ->
                Some typ
            end
          | None ->
            None
        in

        begin
          match structured_init_typ with
          | Some init_typ ->
            let e = (match init with Some e -> e | None -> assert false) in
            let vn = varname_of_ident name in

            add_local_typed b vn init_typ;

            begin
              match init_typ with
              | T.Typ.Array _ ->
                add_var_type b name "__array"
              (* | T.Typ.Struct tn ->
                add_var_type b name (T.TypeName.to_string tn) *)
              | _ ->
                ()
            end;

            let v = lower_expr b e in
            emit b (T.Instr.Store {
              exp1 = T.Exp.Lvar vn;
              typ = Some init_typ;
              exp2 = v;
              loc
            })

          | None ->
            let vn = varname_of_ident name in
            add_local b vn;
            begin
              match init with
              | Some e ->
                let v = lower_expr b e in
                emit b (T.Instr.Store {
                  exp1 = T.Exp.Lvar vn;
                  typ = Some T.Typ.Int;
                  exp2 = v;
                  loc
                })
              | None ->
                ()
            end
        end)

    (* Tuple declaration with initializer.

      lower_expr e returns a tuple value.
      Textual field access needs addressable storage, so copy the tuple value
      into a fresh tuple-typed temporary local and then read fields from that
      temporary storage. *)
    | S_Decl (_, LDI_Tuple names, _ty_opt, Some e) ->
      
      let n = List.length names in
      Hashtbl.replace _emitted_tuple_types n true;

      let tuple_val = lower_expr b e in
      let tmp = materialize_tuple_value_to_lvar b n tuple_val loc in

      List.iteri
        (fun i name ->
          let vn = varname_of_ident name in
          add_local_typed b vn T.Typ.Int;

          let item = load_tuple_field_from_lvar b tmp n i loc in
          emit b (T.Instr.Store {
            exp1 = T.Exp.Lvar vn;
            typ = Some T.Typ.Int;
            exp2 = item;
            loc;
          }))
        names

    (* Tuple declaration without initializer: introduce scalar locals only.

      This is not a tuple value; it is destructuring-style local declaration.
      Each binding is an independent scalar local. *)
    | S_Decl (_, LDI_Tuple names, _ty_opt, None) ->
      List.iter
        (fun name ->
          let vn = varname_of_ident name in
          add_local_typed b vn T.Typ.Int)
        names

    (* Assertion. We model assertion failure as a call to ASL_AssertionFailure on
      the failing branch. The successful branch continues with a Prune. *)
    | S_Assert e ->
      _needs_assert_failure := true;
      let v = lower_expr b e in
      let ok_lbl = fresh_label b "assert_ok" in
      let fail_lbl = fresh_label b "assert_fail" in
      terminate b (mk_jump [ok_lbl; fail_lbl]);
      start_block b fail_lbl;
      emit b (T.Instr.Prune { exp = T.Exp.not v; loc });
      emit b (T.Instr.Let {
        id = None;
        exp = T.Exp.call_non_virtual (mk_procname "ASL_AssertionFailure") [];
        loc });
      terminate b T.Terminator.Unreachable;
      start_block b ok_lbl;
      emit b (T.Instr.Prune { exp = v; loc })

    (* Conditional statement is delegated to lower_if. *)
    | S_Cond (cond, s_then, s_else) ->
      lower_if b cond s_then s_else

    (* While loop skeleton. This is a simple CFG encoding and does not yet model
      loop-limit semantics precisely. *)
    | S_While (cond, _limit, body) ->
      let header_lbl = fresh_label b "while_header" in
      let body_lbl = fresh_label b "while_body" in
      let exit_lbl = fresh_label b "while_exit" in
      terminate b (mk_jump [header_lbl]);
      start_block b header_lbl;
      let cond_val = lower_expr b cond in
      terminate b (mk_jump [body_lbl; exit_lbl]);
      start_block b body_lbl;
      emit b (T.Instr.Prune { exp = cond_val; loc });
      lower_stmt b body;
      if not b.terminated then
        terminate b (mk_jump [header_lbl]);
      start_block b exit_lbl;
      emit b (T.Instr.Prune { exp = T.Exp.not cond_val; loc })

    (* Repeat-until loop skeleton.

      Execute body once, then evaluate cond.  If cond is true, exit;
      otherwise loop back to body.

      If body does not fall through, do not emit the condition block. *)
    | S_Repeat (body, cond, _limit) ->
      let body_lbl = fresh_label b "repeat_body" in
      let cond_lbl = fresh_label b "repeat_cond" in
      let exit_lbl = fresh_label b "repeat_exit" in

      terminate b (mk_jump [body_lbl]);

      start_block b body_lbl;
      lower_stmt b body;

      let body_falls_through = not b.terminated in

      if body_falls_through then begin
        terminate b (mk_jump [cond_lbl]);

        start_block b cond_lbl;
        let cond_val = lower_expr b cond in
        terminate b (mk_jump [exit_lbl; body_lbl]);

        start_block b exit_lbl;
        emit b (T.Instr.Prune { exp = cond_val; loc })
      end

    (* For loop skeleton. Step is +1 for Up and -1 for Down. Loop-limit semantics
      are not fully represented. *)
    | S_For { index_name; start_e; dir; end_e; body; _ } ->
      let vn = varname_of_ident index_name in
      add_local b vn;
      let header_lbl = fresh_label b "for_header" in
      let body_lbl = fresh_label b "for_body" in
      let exit_lbl = fresh_label b "for_exit" in
      let start_val = lower_expr b start_e in
      emit b (T.Instr.Store {
        exp1 = T.Exp.Lvar vn; typ = Some T.Typ.Int;
        exp2 = start_val; loc });
      terminate b (mk_jump [header_lbl]);
      start_block b header_lbl;
      let i_id = fresh_id b in
      emit b (T.Instr.Load {
        id = i_id; exp = T.Exp.Lvar vn;
        typ = Some T.Typ.Int; loc });
      let end_val = lower_expr b end_e in
      let cmp_op = match dir with
        | Up -> IR.Binop.Le
        | Down -> IR.Binop.Ge in
      let cmp_id = fresh_id b in
      emit b (T.Instr.Let {
        id = Some cmp_id;
        exp = T.Exp.call_non_virtual (T.ProcDecl.of_binop cmp_op)
          [T.Exp.Var i_id; end_val];
        loc });
      terminate b (mk_jump [body_lbl; exit_lbl]);
      start_block b body_lbl;
      emit b (T.Instr.Prune { exp = T.Exp.Var cmp_id; loc });
      lower_stmt b body;
      if not b.terminated then begin
        let i_id2 = fresh_id b in
        emit b (T.Instr.Load {
          id = i_id2; exp = T.Exp.Lvar vn;
          typ = Some T.Typ.Int; loc });
        let step_op = match dir with
          | Up -> IR.Binop.PlusA None
          | Down -> IR.Binop.MinusA None in
        let next_id = fresh_id b in
        emit b (T.Instr.Let {
          id = Some next_id;
          exp = T.Exp.call_non_virtual (T.ProcDecl.of_binop step_op)
            [T.Exp.Var i_id2; T.Exp.Const (T.Const.Int Z.one)];
          loc });
        emit b (T.Instr.Store {
          exp1 = T.Exp.Lvar vn; typ = Some T.Typ.Int;
          exp2 = T.Exp.Var next_id; loc });
        terminate b (mk_jump [header_lbl])
      end;
      start_block b exit_lbl;
      emit b (T.Instr.Prune { exp = T.Exp.not (T.Exp.Var cmp_id); loc })

    (* Pragmas do not affect current SIL model. *)
    | S_Pragma _ ->
      ()

    | S_Print { args; _ } ->
      (* Print/println have no analysis-visible output effect in this frontend.
        Still lower non-display-only arguments so any calls, reads, asserts, or
        other side effects inside print arguments are preserved.

        String and real literals are display-only here. Lowering them globally as
        fake integers would pollute expression semantics, so ignore them only in
        print contexts. *)
      List.iter
        (fun e ->
          match e.desc with
          | E_Literal (L_String _)
          | E_Literal (L_Real _) ->
            ()
          | _ ->
            ignore (lower_expr b e))
        args

    (* Unreachable statement: emit a helper call, then terminate. This may need a
      more precise Textual terminator if available. *)
    | S_Unreachable ->
      _needs_unreachable := true;
      emit b (T.Instr.Let {
        id = None;
        exp = T.Exp.call_non_virtual (mk_procname "ASL_Unreachable") [];
        loc
      });
      terminate_return b None

    (* Procedure call statement: lower args, discard result. *)
    | S_Call { name; args; _ } ->
      let arg_vals = List.map (lower_expr b) args in
      let arity = List.length args in

      if not (Hashtbl.mem _defined_procs (name, arity)) then
        Hashtbl.replace _external_calls (name, arity) ();

      emit b (T.Instr.Let {
        id = None;
        exp = T.Exp.call_non_virtual (mk_procname name) arg_vals;
        loc
      })

    (* Textual throw expects a pointer/address to the exception object.

      The address here is the address of temporary/local storage used 
      to transport the thrown value through Textual's exception mechanism. *)
    | S_Throw (e, _ty_opt) ->
      let exn_lvar = lower_exception_expr_to_lvar b e loc in
      terminate b (T.Terminator.Throw (T.Exp.Lvar exn_lvar))

    (* Try/catch lowering. This is coarse: it wires throws in body to a
      synthetic catch block, then continues at after_try. Multiple catchers and
      otherwise are simplified. *)
    | S_Try (body, catchers, otherwise) ->
      let catch_lbl = fresh_label b "catch" in
      let after_lbl = fresh_label b "after_try" in
      let saved_exn = b.exn_succs in
      b.exn_succs <- [catch_lbl];
      lower_stmt b body;
      if not b.terminated then
        terminate b (mk_jump [after_lbl]);
      b.exn_succs <- saved_exn;
      start_block b catch_lbl;
      (match catchers with
      | [(name_opt, _ty, catch_body)] ->
        (match name_opt with
          | Some name ->
            let vn = varname_of_ident name in
            add_local b vn
          | None -> ());
        lower_stmt b catch_body;
        if not b.terminated then
          terminate b (mk_jump [after_lbl])
      | _ ->
        (match catchers with
          | (_, _, catch_body) :: _ ->
            lower_stmt b catch_body;
            if not b.terminated then
              terminate b (mk_jump [after_lbl])
          | [] ->
            (match otherwise with
            | Some other_body ->
              lower_stmt b other_body;
              if not b.terminated then
                terminate b (mk_jump [after_lbl])
            | None ->
              terminate b (mk_jump [after_lbl]))));
      start_block b after_lbl

    (* Assignment. RHS is evaluated exactly once before storing. lower_store_lexpr
       then lowers the LHS storage/update form using that RHS value; it may evaluate
       LHS subexpressions such as indexes/slices/bases, but must not re-evaluate RHS. *)
    | S_Assign (le, e) ->
      let old_expected = !_expected_call_return_tuple_arity in

      begin
        match le.desc with
        | LE_Destructuring les ->
          _expected_call_return_tuple_arity := Some (List.length les)
        | _ ->
          _expected_call_return_tuple_arity := None
      end;

      let v =
        try lower_expr b e
        with exn ->
          _expected_call_return_tuple_arity := old_expected;
          raise exn
      in
      _expected_call_return_tuple_arity := old_expected;

      lower_store_lexpr b le v loc

(* --- If/else lowering --- *)

(* Lower an if/else statement to two successor blocks plus an optional merge.

   Textual branch conditions are represented by Prune instructions in each
   successor. If both branches terminate, no merge block is created. *)
and lower_if b (cond : AST.expr) (s_then : AST.stmt) (s_else : AST.stmt) =
  let cond_val = lower_expr b cond in
  let loc = b.last_loc in
  let then_lbl = fresh_label b "then" in
  let else_lbl = fresh_label b "else" in
  let merge_lbl = fresh_label b "merge" in
  let all_term = ref true in
  terminate b (mk_jump [then_lbl; else_lbl]);
  start_block b then_lbl;
  emit b (T.Instr.Prune { exp = cond_val; loc });
  lower_stmt b s_then;
  if not b.terminated then begin
    all_term := false;
    terminate b (mk_jump [merge_lbl])
  end;
  start_block b else_lbl;
  emit b (T.Instr.Prune { exp = T.Exp.not cond_val; loc });
  lower_stmt b s_else;
  if not b.terminated then begin
    all_term := false;
    terminate b (mk_jump [merge_lbl])
  end;
  if not !all_term then
    start_block b merge_lbl

(* --- Function and program lowering --- *)

(* Comma-separated list of ASL functions to emit as declarations only. This is
   useful when the frontend sees calls to primitives or implementation-provided
   helpers that are not lowered from ASL bodies. *)
let external_funcs =
  try String.split_on_char ',' (Sys.getenv "ASL_EXTERNALS")
  with Not_found -> []

let is_external name = List.mem name external_funcs

let lower_func (f : AST.func) : T.Module.decl option =
  match f.body with
  | SB_Primitive _ -> None
  | SB_ASL body ->
    let b = create_builder () in
    b.current_func <- f.name;
    b.func_args <- f.args;

    (* Parameter type lowering. Most scalar-like ASL types are currently int.
       Records/exceptions/named record types are value structs. *)
    let lower_param_type (_, ty) = lower_ty_annotated ty in
    let formals_types = List.map lower_param_type f.args in

    (* Return type lowering. Tuple and record returns use value structs. Void is
       represented as Textual Void and statement lowering consults b.result_type
       for returns. *)
    let result_type = match f.return_type with
      | Some ty -> lower_ty_annotated ty
      | None -> mk_typ_annotated T.Typ.Void
    in
    b.result_type <- result_type.typ;
    let procdecl = T.ProcDecl.{
      qualified_name = mk_procname f.name;
      formals_types = Some formals_types;
      result_type;
      attributes = [T.Attr.mk_static];
    } in
    if is_external f.name then
      Some (T.Module.Procdecl procdecl)
    else begin
      lower_stmt b body;
      if not b.terminated then
        terminate_return b None;
      let procdesc = T.ProcDesc.{
        procdecl;
        nodes = b.nodes;
        fresh_ident = None;
        start = entry_label;
        params = List.map (fun (n, _) -> varname_of_ident n) f.args;
        locals = b.locals;
        exit_loc = loc_unknown;
      } in
      Some (T.Module.Proc procdesc)
    end

(* Global storage declaration.

   The declared global type is lowered with lower_ty, so record/tuple/array
   globals can keep their Textual storage type.  Global initializers are currently
   ignored: this is an approximation because global setup side effects and initial 
   values are not represented in the emitted module. *)

let lower_global (gsd : AST.global_decl) : T.Module.decl option =
  let typ =
    match gsd.ty with
    | Some ty -> lower_ty ty
    | None -> T.Typ.Int
  in
  Some (T.Module.Global {
    T.Global.name = T.VarName.of_string gsd.name;
    typ;
    attributes = [];
    init_exp = None;
  })

(* Declaration for a callee whose body is not emitted in this
   Textual module.

   Unknown ASL primitives are currently modelled as int-returning functions with 
   int arguments.
   *)
let external_call_decl name arity result_typ =
  T.Module.Procdecl T.ProcDecl.{
    qualified_name = mk_procname name;
    formals_types =
      Some (List.init arity (fun _ -> mk_typ_annotated T.Typ.Int));
    result_type = mk_typ_annotated result_typ;
    attributes = [];
  }

let external_global_decl name =
  T.Module.Global {
    T.Global.name = T.VarName.of_string name;
    typ = T.Typ.Int;
    attributes = [];
    init_exp = None;
  }

let int1_to_int_decl name =
  T.Module.Procdecl T.ProcDecl.{
    qualified_name = mk_procname name;
    formals_types = Some [mk_typ_annotated T.Typ.Int];
    result_type = mk_typ_annotated T.Typ.Int;
    attributes = [];
  }

let int2_to_int_decl name =
  T.Module.Procdecl T.ProcDecl.{
    qualified_name = mk_procname name;
    formals_types = Some [
      mk_typ_annotated T.Typ.Int;
      mk_typ_annotated T.Typ.Int;
    ];
    result_type = mk_typ_annotated T.Typ.Int;
    attributes = [];
  }

let int3_to_int_decl name =
  T.Module.Procdecl T.ProcDecl.{
    qualified_name = mk_procname name;
    formals_types = Some [
      mk_typ_annotated T.Typ.Int;
      mk_typ_annotated T.Typ.Int;
      mk_typ_annotated T.Typ.Int;
    ];
    result_type = mk_typ_annotated T.Typ.Int;
    attributes = [];
  }

let int4_to_int_decl name =
  T.Module.Procdecl T.ProcDecl.{
    qualified_name = mk_procname name;
    formals_types = Some [
      mk_typ_annotated T.Typ.Int;
      mk_typ_annotated T.Typ.Int;
      mk_typ_annotated T.Typ.Int;
      mk_typ_annotated T.Typ.Int;
    ];
    result_type = mk_typ_annotated T.Typ.Int;
    attributes = [];
  }

(* Lower one ASL source file/module to a Textual module.

   Only declarations whose source filename matches [filename] are emitted. The
   stdlib may be present in the typed AST, but this avoids dumping all stdlib
   declarations into every generated module. *)
let lower_program filename (ast : AST.t) : T.Module.t =
  (* Reset module-level mutable state. These tables/flags are
     per-module, not global across invocations. *)
  _needs_arbitrary := false;
  _needs_assert_failure := false;
  _needs_unreachable := false;
  _needs_slice_helper := false;
  _needs_bv_concat_helper := false;
  _needs_pow_helper := false;
  _needs_divrm_helper := false;
  _needs_rdiv_helper := false;
  _needs_str_concat_helper := false;
  _needs_bv_slice_length_helper := false;
  _needs_bv_slice_range_helper := false;
  _needs_bv_slice_single_helper := false;
  _needs_bv_slice_multi_helper := false;
  _needs_bv_update_length_helper := false;
  _needs_bv_update_range_helper := false;
  _needs_bv_update_single_helper := false;
  _needs_bv_update_multi_helper := false;
  _needs_bool_impl_helper := false;
  Hashtbl.clear _emitted_tuple_types;
  Hashtbl.clear _type_decl_names;
  Hashtbl.clear _type_decl_defs;
  Hashtbl.clear _struct_field_types;
  register_type_decls ast;
  Hashtbl.clear _defined_procs;
  Hashtbl.clear _external_calls;
  Hashtbl.clear _defined_globals;
  Hashtbl.clear _external_globals;
  Hashtbl.clear _func_return_tuple_arity;
  Hashtbl.clear _external_return_tuple_arity;
  _expected_call_return_tuple_arity := None;

  (* Register only functions that this module will actually emit.

     Do not register all D_Funcs in the typed AST, because the typed AST may
     include stdlib/primitive functions whose bodies are not emitted here.  If
     we registered those, they would not get a Procdecl and Textual
     verification would fail. *)
  List.iter
    (fun (d : AST.decl) ->
      if d.pos_start.pos_fname = filename then
        match d.desc with
        | D_Func f ->
          Hashtbl.replace _defined_procs (f.name, List.length f.args) ();
          begin
            (* Why is return type only matched with Tuple? *)
            match f.return_type with
            | Some { desc = T_Tuple ts; _ } ->
              Hashtbl.replace _func_return_tuple_arity f.name (List.length ts)
            | _ ->
              ()
          end

        | D_GlobalStorage gsd ->
          Hashtbl.replace _defined_globals gsd.name ()

        | _ ->
          ())
  ast;
  let decls = List.filter_map (fun (d : AST.decl) ->
    if d.pos_start.pos_fname <> filename then None
    else match d.desc with
    | D_Func f -> lower_func f
    | D_GlobalStorage gsd -> lower_global gsd
    | D_TypeDecl (name, ty, _) -> lower_type_decl name ty
    | _ -> None
  ) ast in
  let external_call_decls =
    Hashtbl.fold
      (fun (name, arity) () acc ->
        if Hashtbl.mem _defined_procs (name, arity) then acc
        else
          let result_typ =
            match Hashtbl.find_opt _external_return_tuple_arity (name, arity) with
            | Some n ->
              Hashtbl.replace _emitted_tuple_types n true;
              (* Printf.eprintf "tuple external %s/%d -> tuple_%d\n%!" name arity n; *)
              tuple_struct_typ n
            | None ->
              T.Typ.Int
          in
          external_call_decl name arity result_typ :: acc)
      _external_calls
      []
  in
  let external_global_decls =
  Hashtbl.fold
    (fun name () acc ->
      if Hashtbl.mem _defined_globals name then acc
      else external_global_decl name :: acc)
    _external_globals
    []
  in
  let tuple_decls = Hashtbl.fold (fun n _ acc ->
    tuple_struct_decl n :: acc
  ) _emitted_tuple_types [] in

  let extra_decls =
    (* Synthetic helper declarations.  These helpers are declared
       but not defined by this frontend. Treat them as semantic holes:
       useful for preserving operation boundaries, but low-fidelity unless a
       downstream model provides their behavior. *)
    (if !_needs_slice_helper then
      [T.Module.Procdecl T.ProcDecl.{
        qualified_name = mk_procname "__asl_slice";
        formals_types = Some [
          mk_typ_annotated T.Typ.Int;
          mk_typ_annotated T.Typ.Int;
          mk_typ_annotated T.Typ.Int;
        ];
        result_type = mk_typ_annotated T.Typ.Int;
        attributes = [];
      }]
    else [])
    @
    (if !_needs_bv_concat_helper then
      [T.Module.Procdecl T.ProcDecl.{
        qualified_name = mk_procname "__asl_bv_concat";
        formals_types = Some [
          mk_typ_annotated T.Typ.Int;
          mk_typ_annotated T.Typ.Int;
        ];
        result_type = mk_typ_annotated T.Typ.Int;
        attributes = [];
      }]
    else [])
    @
    (if !_needs_pow_helper then [int2_to_int_decl "__asl_pow"] else [])
    @
    (if !_needs_divrm_helper then [int2_to_int_decl "__asl_divrm"] else [])
    @
    (if !_needs_rdiv_helper then [int2_to_int_decl "__asl_rdiv"] else [])
    @
    (if !_needs_str_concat_helper then [int2_to_int_decl "__asl_str_concat"] else [])
    @
    (if !_needs_arbitrary then
      [T.Module.Procdecl T.ProcDecl.{
        qualified_name = mk_procname "ASL_Arbitrary";
        formals_types = Some [];
        result_type = mk_typ_annotated T.Typ.Int;
        attributes = [];
      }]
    else [])
    @
    (if !_needs_assert_failure then
      [T.Module.Procdecl T.ProcDecl.{
        qualified_name = mk_procname "ASL_AssertionFailure";
        formals_types = Some [];
        result_type = mk_typ_annotated T.Typ.Void;
        attributes = [];
      }]
    else [])
    @
    (if !_needs_unreachable then
      [T.Module.Procdecl T.ProcDecl.{
        qualified_name = mk_procname "ASL_Unreachable";
        formals_types = Some [];
        result_type = mk_typ_annotated T.Typ.Void;
        attributes = [];
      }]
    else [])
    @
    (if !_needs_bv_slice_single_helper then
      [int2_to_int_decl "ASL_BVSliceSingle"]
    else [])
    @
    (if !_needs_bv_slice_length_helper then
      [int3_to_int_decl "ASL_BVSliceLength"]
    else [])
    @
    (if !_needs_bv_slice_range_helper then
      [int3_to_int_decl "ASL_BVSliceRange"]
    else [])
    @
    (if !_needs_bv_update_single_helper then
      [int3_to_int_decl "ASL_BVUpdateSingle"]
    else [])
    @
    (if !_needs_bv_update_length_helper then
      [int4_to_int_decl "ASL_BVUpdateLength"]
    else [])
    @
    (if !_needs_bv_update_range_helper then
      [int4_to_int_decl "ASL_BVUpdateRange"]
    else [])
    @
    (if !_needs_bv_update_multi_helper then
      [int2_to_int_decl "ASL_BVUpdateMulti"]
    else [])
    @
    (if !_needs_bv_slice_multi_helper then
      [int1_to_int_decl "ASL_BVSliceMulti"]
    else [])
    @
    (if !_needs_bool_impl_helper then
      [int2_to_int_decl "ASL_BoolImpl"]
    else [])
  in
  let sourcefile = T.SourceFile.create filename in
  T.Module.{
    attrs = [T.Attr.mk_source_language T.Lang.C];
    decls = tuple_decls @ extra_decls @ external_call_decls @ external_global_decls @ decls;
    sourcefile;
  }

(* --- Direct capture to Infer DB --- *)

(* Verify, transform, convert to SIL, and store in Infer's global tenv/DB.

   This path runs Textual verification and Textual->SIL conversion, so failures 
   here usually indicate type/signature/CFG problems in the lowering rather than 
   unsupported ASL syntax. *)
let capture_to_db (module_ : T.Module.t) =
  let sourcefile = module_.sourcefile in
  match Textuallib.TextualVerification.verify_strict module_ with
  | Error errors ->
    List.iter (fun e ->
      Format.eprintf "Verification error: %a@."
        (Textuallib.TextualVerification.pp_error_with_sourcefile sourcefile) e
    ) errors;
    raise (Lowering_error TextualVerificationFailed)
  | Ok verified ->
    let transformed, decls =
      Textuallib.TextualTransform.run_exn T.Lang.C verified in
    (match Textuallib.TextualSil.module_to_sil T.Lang.C transformed decls with
    | Error errors ->
      List.iter (fun e ->
        Format.eprintf "SIL error: %a@."
          (T.pp_transform_error sourcefile) e
      ) errors;
      raise (Lowering_error TextualToSilFailed)
    | Ok (cfg, tenv) ->
      let sil = { Textuallib.TextualParser.TextualFile.
        sourcefile; cfg; tenv } in
      Textuallib.TextualParser.TextualFile.capture
        ~textual_module:transformed ~use_global_tenv:false sil ;
      IR.Tenv.Global.store ~normalize:true tenv)
