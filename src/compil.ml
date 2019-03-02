module S = Typed_ast
module T = Cpp_ast

let fonction (f:S.t_node) : T.cpp_fun =
  { T.cpp_name = f.S.tn_name
  ; T.cpp_input = f.S.tn_input
  ; T.cpp_output = f.S.tn_output
  ; T.cpp_local = f.S.tn_local
  ; T.cpp_affs = f.S.tn_equs
  }

let main ft =
  List.map fonction ft
