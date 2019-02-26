module T = Typed_ast

let fonction f =
  { tn_name = f.tn_name
  ; tn_input = f.tn_input
  ; tn_output = f.tn_output
  ; tn_local = f.tn_local
  ; tn_equs = f.tn_equs
  ; tn_loc = f.tn_loc
  }

let main ft =
  List.map fonction ft
