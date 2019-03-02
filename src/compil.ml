module S = Typed_ast
module T = Cpp_ast
module I = Ident


let rec expr = function
  | S.TE_const c -> T.CPP_const c
  | S.TE_ident i -> T.CPP_ident i.I.name
  | S.TE_op (op,el) -> T.CPP_op (op,List.map (fun x -> expr x.S.texpr_desc) el)
  | S.TE_app (i,el) -> T.CPP_app (i.I.name,List.map (fun x -> expr x.S.texpr_desc) el)
  | S.TE_prim (i,el) -> T.CPP_prim (i.I.name,List.map (fun x -> expr x.S.texpr_desc) el)
  | S.TE_arrow (e1,e2) -> T.CPP_arrow (expr e1.S.texpr_desc,expr e2.S.texpr_desc)
  | S.TE_pre e -> T.CPP_pre (expr e.S.texpr_desc)
  | S.TE_tuple el -> T.CPP_tuple (List.map (fun x -> expr x.S.texpr_desc) el)

let fonction (f:S.t_node) : T.cpp_fun =


  let new_name = f.S.tn_name.I.name in
  let new_input = List.map
      (fun x -> ( (fst x).I.name, snd x) ) f.S.tn_input in
  let new_output = List.map
      (fun x -> ( (fst x).I.name, snd x) ) f.S.tn_output in
  let new_local = List.map
      (fun x -> ( (fst x).I.name, snd x) ) f.S.tn_local in



  let ff x = List.map (fun y -> y.I.name) x.S.tpatt_desc in


  let gg x = expr x.S.texpr_desc in


  let new_equs = List.map (
        fun x -> { T.cppeq_patt = ff x.S.teq_patt ; T.cppeq_expr = gg x.S.teq_expr }
      ) f.S.tn_equs in


  { T.cpp_name = new_name
  ; T.cpp_input = new_input
  ; T.cpp_output = new_output
  ; T.cpp_local = new_local
  ; T.cpp_affs = new_equs
  }

let main ft =
  List.map fonction ft
