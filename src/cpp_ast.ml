(* Arbres de syntaxe abstraite C++ *)

open Asttypes

type typed_var = string * base_ty

and cpp_expr =
  | CPP_const  of const
  | CPP_ident  of string
  | CPP_op     of op * cpp_expr list
  | CPP_app    of string * cpp_expr list
  | CPP_prim   of string * cpp_expr list
  | CPP_arrow  of cpp_expr * cpp_expr
  | CPP_pre    of cpp_expr
  | CPP_tuple  of cpp_expr list

type cpp_affect =
  { cppeq_patt: string list
  ; cppeq_expr: cpp_expr
  }

(* Les fonctions lustre sont représentées par des lambda fonctions,
 * qui permettent non seulement de modéliser le traitement d'un
 * noeud mais également son environnement grâce aux lambda captures
 * mutables.
 *)
type cpp_fun =
  { cpp_name:   string
  ; cpp_input:  typed_var list
  ; cpp_output: typed_var list
  ; cpp_local:  typed_var list
  ; cpp_affs:   cpp_affect list
  }

type cpp_file = cpp_fun list
