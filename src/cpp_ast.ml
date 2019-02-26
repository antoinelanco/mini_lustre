(* Arbres de syntaxe abstraite C++ *)

open Asttypes

type typed_var = Ident.t * base_ty

type cpp_expr =
  { cppexpr_desc: cpp_expr_desc
  ; cppexpr_type: ty
  }

and cpp_expr_desc =
  | CPP_const  of const
  | CPP_ident  of Ident.t
  | CPP_op     of op * cpp_expr list
  | CPP_app    of Ident.t * cpp_expr list
  | CPP_prim   of Ident.t * cpp_expr list
  | CPP_arrow  of cpp_expr * cpp_expr
  | CPP_pre    of cpp_expr
  | CPP_tuple  of cpp_expr list

type t_patt =
  { cpppatt_desc: Ident.t list
  ; cpppatt_type: ty
  }

type t_equation =
  { cppeq_patt: t_patt
  ; cppeq_expr: cpp_expr
  }

type cpp_fun =
  { cpp_name:   Ident.t
  ; cpp_input:  typed_var list
  ; cpp_output: typed_var list
  ; cpp_local:  typed_var list
  ; cpp_equs:   t_equation list
  }

type cpp_file = cpp_fun list
