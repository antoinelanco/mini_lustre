module C = Cpp_ast

open Asttypes

(* Formats a list of variables with a prefix, a separator, an end
 * suffix
 *)
let format_var_list (prefix:string) (separator:string) (suffix:string)
  (t_var_trans: C.typed_var -> string) (t_vars: C.typed_var list)
: string =
  let var_list = List.map t_var_trans t_vars in
  let formatted_list =
    match var_list with
    | []    ->  ""
    | e::tl -> (List.fold_left (fun acc elmt -> acc ^ separator ^ elmt) e tl)
    in
  prefix ^ formatted_list ^ suffix

(* Prints local variables stored in the node *)
let print_locals = format_var_list "[" ", " "]"
  ( function
    | s, Tint   -> s ^ " = int{0}"
    | s, Tbool  -> s ^ " = bool{0}"
    | s, Treal  -> s ^ " = float{0}"
  )

(* Prints argument list *)
let print_arguments = format_var_list "(" ", " ")"
  ( function
    | s, Tint   -> "int const& " ^ s
    | s, Tbool  -> "bool const& " ^ s
    | s, Treal  -> "float const& " ^ s
  )

let print_affect (aff: C.cpp_affect): string = ""

let print_lambda_header (f: C.cpp_fun) : string = ""

let print_fun (f: C.cpp_fun): string = ""

let print_file (file: C.cpp_file): string = ""
