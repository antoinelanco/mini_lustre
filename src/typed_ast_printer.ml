(* code d'Adrien Guatto *)

open Format
open Asttypes
open Typed_ast

let rec print_list f sep fmt l = match l with
  | [] -> ()
  | [x] -> f fmt x
  | h :: t -> fprintf fmt "%a%s@ %a" f h sep (print_list f sep) t

let rec print_list_eol f sep fmt l = match l with
  | [] -> ()
  | [x] -> fprintf fmt "%a%s" f x sep
  | h :: t -> fprintf fmt "%a%s@\n%a" f h sep (print_list_eol f sep) t

let print_const fmt c = match c with
  | Cbool b -> fprintf fmt "%b" b
  | Cint i -> fprintf fmt "%d" i
  | Creal f -> fprintf fmt "%f" f

let print_op fmt op = match op with
  | Op_eq -> fprintf fmt "eq"
  | Op_neq -> fprintf fmt "neq"
  | Op_lt -> fprintf fmt "lt"
  | Op_le -> fprintf fmt "le"
  | Op_gt -> fprintf fmt "gt"
  | Op_ge -> fprintf fmt "ge"
  | Op_add -> fprintf fmt "add"
  | Op_sub -> fprintf fmt "sub"
  | Op_mul -> fprintf fmt "mul"
  | Op_div -> fprintf fmt "div"
  | Op_mod -> fprintf fmt "mod"
  | Op_add_f -> fprintf fmt "add_f"
  | Op_sub_f -> fprintf fmt "sub_f"
  | Op_mul_f -> fprintf fmt "mul_f"
  | Op_div_f -> fprintf fmt "div_f"
  | Op_not -> fprintf fmt "~"
  | Op_and -> fprintf fmt "and"
  | Op_or -> fprintf fmt "or"
  | Op_impl -> fprintf fmt "impl"
  | Op_if -> fprintf fmt "ite"

let rec print_exp fmt e = match e.texpr_desc with
  | TE_const c -> print_const fmt c
  | TE_ident x -> fprintf fmt "%a" Ident.print x
  | TE_op (op, el) -> fprintf fmt "%a(%a)" print_op op print_arg_list el
  | TE_app (name, e_list) | TE_prim (name, e_list) ->
      fprintf fmt "%a(@[%a@])" Ident.print name print_arg_list e_list
  | TE_arrow (l, r) ->
      fprintf fmt "@[(@[%a@]) -> (@[%a@])@]" print_exp l print_exp r
  | TE_pre e ->
      fprintf fmt "pre (@[%a@])" print_exp e
  | TE_tuple e_list ->
      fprintf fmt "(@[%a@])" print_tuple_arg_list e_list

and print_arg_list fmt e_list = match e_list with
  | [] -> ()
  | [x] -> fprintf fmt "%a" print_exp x
  | h :: t -> fprintf fmt "%a,@ %a" print_exp h print_arg_list t

and print_tuple_arg_list fmt e_list = match e_list with
  | [] -> assert false
  | [x] -> fprintf fmt "%a" print_exp x
  | h :: t -> fprintf fmt "%a,@ %a" print_exp h print_arg_list t

and print_const_exp fmt ce_list = match ce_list with
  | [] -> assert false
  | [c] -> fprintf fmt "%a" print_const c
  | h :: t -> fprintf fmt "%a,@ %a" print_const h print_const_exp t

let print_eq fmt eq =
  fprintf fmt "@[(%a) = @[%a@]@]"
    (print_list Ident.print ",") eq.teq_patt.tpatt_desc
    print_exp eq.teq_expr

let print_base_type fmt bty = match bty with
  | Tbool -> fprintf fmt "bool"
  | Tint -> fprintf fmt "int"
  | Treal -> fprintf fmt "real"

(* let print_type = print_list print_cbase_type "*" *)

let print_var_dec fmt (name, ty) =
  fprintf fmt "%a : %a" Ident.print name print_base_type ty

let rec print_var_dec_list = print_list print_var_dec ";"

let print_node fmt nd =
  fprintf fmt
    "@[node %a(@[%a@]) returns (@[%a@])@\nvar @[%a;@]@\n@[<v 2>let@ @[%a@]@]@\ntel@]"
    Ident.print nd.tn_name
    print_var_dec_list nd.tn_input
    print_var_dec_list nd.tn_output
    print_var_dec_list nd.tn_local
    (print_list_eol print_eq ";") nd.tn_equs

let print_node_list_std ndl =
  List.iter (fun nd -> Format.printf "%a@\n@." print_node nd) ndl
