module C = Cpp_ast

(* Prints local variables stored in the node *)
let print_locals (t_vars: C.typed_var list): string =
  let capture_list = (List.map (fun elmt ->
    match elmt with
    | s, Tint   -> s ^ " = int{0}"
    | s, Tbool  -> s ^ " = bool{0}"
    | s, Treal  -> s ^ " = float{0}"
  ) t_vars)
  in
  match capture_list with
  | []    -> "[]"
  | e::tl ->
    "[ " ^ (List.fold_left (fun acc elmt -> acc ^ " , " ^ elmt) e tl) ^ " ]"


(* Prints local variables *)
let print_arguments (t_vars: C.typed_var list): string =
  let arg_list = (List.map (fun elmt ->
    match elmt with
    | s, Tint   -> "int const& " ^ s
    | s, Tbool  -> "bool const& " ^ s
    | s, Treal  -> "float const& " ^ s
  ) t_vars)
  in
  match capture_list with
  | []    -> "[]"
  | e::tl ->
    "( " ^ (List.fold_left (fun acc elmt -> acc ^ " , " ^ elmt) e tl) ^ " )"

let reformat_var_list
  (beg_str:string) (sep_str:string) (end_str:string)
  (t_var_trans: C.typed_var -> string)
  (t_vars: C.typed_var list)
: string =
  let var_list = List.map t_var_trans t_vars in
  let formatted_list =
  match var_list with
  | []    ->  ""
  | e::tl -> (List.fold_left (fun acc elmt -> acc ^ sep_str ^ elmt) e tl)
  in
  beg_str ^ formatted_list ^ end_str

let print_affect (aff: C.cpp_affect): string = ""

let print_lambda_header (f: C.cpp_fun) : string = ""

let print_fun (f: C.cpp_fun): string = ""

let print_file (file: C.cpp_file): string = ""
