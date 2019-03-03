module C = Cpp_ast

open Asttypes

(* Formats a list of variables with a prefix, a separator, an end
 * suffix
 *)
let format_list (prefix:string) (separator:string) (suffix:string)
  (trans: 'a -> string) (ls: 'a list)
: string =
  let transformed_ls = List.map trans ls in
  let formatted_list =
    match transformed_ls with
    | []    ->  ""
    | e::tl -> (List.fold_left (fun acc elmt -> acc ^ separator ^ elmt) e tl)
    in
  prefix ^ formatted_list ^ suffix

(* Prints local variables stored in the node *)
let var_list_to_capture = format_list "[" ", " "]" (function
    | s, Tint   -> s ^ " = int{0}"
    | s, Tbool  -> s ^ " = bool{0}"
    | s, Treal  -> s ^ " = float{0}"
  )

(* Prints argument list *)
let var_list_to_arguments = format_list "(" ", " ")" (function
    | s, Tint   -> "int const& " ^ s
    | s, Tbool  -> "bool const& " ^ s
    | s, Treal  -> "float const& " ^ s
  )

let var_list_to_tie = format_list "std::tie(" ", " ")" (function s -> s)

let print_const = function
  | Cbool (v) -> string_of_bool v
  | Cint  (v) -> string_of_int v
  | Creal (v) -> string_of_float v

let print_op_symbol = function
  | Op_eq    -> "==" | Op_neq   -> "!=" | Op_lt    -> "<"  | Op_le    -> "<="
  | Op_gt    -> ">"  | Op_ge    -> ">=" | Op_add   -> "+"  | Op_sub   -> "-"
  | Op_mul   -> "*"  | Op_div   -> "/"  | Op_mod   -> "%"  | Op_add_f -> "+"
  | Op_sub_f -> "-"  | Op_mul_f -> "*"  | Op_div_f -> "/"  | Op_not   -> "!"
  | Op_and   -> "&&" | Op_or    -> "||"
  | _ -> failwith "Not printable"

let rec print_exp (exp: C.cpp_expr): string =
  (* Tuple printer *)
  let expr_list_to_tuple =
    format_list "std::forward_as_tuple(" ", " ")" print_exp
  in

  (* Argument list printer *)
  let expr_list_to_args =
    format_list "(" ", " ")" print_exp
  in

  let print_op op elist =
    match op, elist with
    | Op_eq    | Op_neq   | Op_lt    | Op_le    | Op_gt  | Op_ge
    | Op_add   | Op_sub   | Op_mul   | Op_div   | Op_mod
    | Op_add_f | Op_sub_f | Op_mul_f | Op_div_f
    | Op_not
    | Op_and   | Op_or    | Op_impl
    | Op_if

  match exp with
  | CPP_const  (c)            -> print_const c
  | CPP_ident  (id)           -> id
  | CPP_op     (op, elist)    -> failwith "Not implemented"
  | CPP_tuple  (elist)        -> expr_list_to_tuple elist
  | CPP_app    (name, elist)
  | CPP_prim   (name, elist)  -> name ^ expr_list_to_args elist

  (*| CPP_arrow  of cpp_expr * cpp_expr*)
  (*| CPP_pre    of cpp_expr*)

  | _ -> failwith "Not implemented"

let print_affect (aff: C.cpp_affect): string =
  let exp = print_exp aff.C.cppeq_expr in
  match aff.C.cppeq_patt with
  | []    -> failwith "Error: empty affectation"
  | [id]  -> id ^ " = " ^ exp ^ ";"
  | _     -> (var_list_to_tie aff.C.cppeq_patt) ^ " = " ^ exp ^ ";"

let print_lambda_header (f: C.cpp_fun) : string = ""

let print_fun (f: C.cpp_fun): string = ""

let print_file (file: C.cpp_file): string = ""
