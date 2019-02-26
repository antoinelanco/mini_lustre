open Asttypes
open Parse_ast
open Typed_ast
open Format

module S = Set.Make(Ident)
module M = Map.Make(String)


type error =
  | ExpectedType of ty * ty
  | ExpectedPattern of ty
  | ExpectedBase of ty
  | ExpectedNum of ty
  | UnboundVar of string
  | UnboundNode of string
  | TooFewArguments
  | TooManyArguments
  | Clash of string
  | ConstantExpected
  | Other of string
  | FlatTuple
  | UndefinedOutputs of string list
  | InputVar of string
  | Causality
  | BadMain of ty * ty

exception Error of location * error
let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

let error loc e = raise (Error (loc, e))
let errors loc s = error loc (Other s)

let print_base_type fmt = function
  | Tbool -> fprintf fmt "bool"
  | Tint -> fprintf fmt "int"
  | Treal -> fprintf fmt "real"

let print_type fmt = function
  | ([]) -> fprintf fmt "empty tuple"
  | [t] -> print_base_type fmt t
  | (t::tl) ->
      fprintf fmt "(";
      print_base_type fmt t;
      List.iter (fun t -> fprintf fmt " * %a" print_base_type t) tl;
      fprintf fmt ")"

let report fmt = function
  | UnboundVar id -> fprintf fmt "unbound variable %s" id
  | UnboundNode id -> fprintf fmt "unbound node %s" id
  | ExpectedType (t1,t2) ->
      fprintf fmt
      "this expression has type %a but is expected to have type %a"
      print_type t1 print_type t2
  | ExpectedPattern ty ->
      fprintf fmt "this pattern is expected to have type %a"
	print_type ty
  | ExpectedBase ty ->
      fprintf fmt
     "this expression has type %a but is expected to have a type simple type"
      print_type ty
  | ExpectedNum ty ->
      fprintf fmt
      "this expression has type %a but is expected to have type int or real"
      print_type ty
  | Clash id -> fprintf fmt "The variable %s is defined several times" id
  | TooFewArguments -> fprintf fmt "too few arguments"
  | TooManyArguments -> fprintf fmt "too many arguments"
  | ConstantExpected -> fprintf fmt "this expression sould be a constant"
  | Other s -> fprintf fmt "%s" s
  | FlatTuple -> fprintf fmt "nested tuples are forbidden"
  | UndefinedOutputs l ->
      fprintf fmt "those output variables are undefined:%a"
	(fun fmt -> List.iter (fun x -> fprintf fmt "%s " x)) l
  | InputVar s -> fprintf fmt "%s is an input variable" s
  | Causality -> fprintf fmt "problem of causality"
  | BadMain (t_in, t_out) ->
      fprintf fmt "The main node has type %a -> %a but is expected to have type %a -> bool"
	print_type t_in print_type t_out
        print_type t_in

let int_of_real = Ident.make "int_of_real" Ident.Prim
let real_of_int = Ident.make "real_of_int" Ident.Prim

module Delta = struct

  let prims = [
    "int_of_real", (int_of_real, ([Treal] , [Tint])) ;
    "real_of_int", (real_of_int, ([Tint] , [Treal])) ; ]

  let nodes = Hashtbl.create 97

  let is_primitive f = List.mem_assoc f prims

  let find n =
    try Hashtbl.find nodes n , false with
	Not_found -> List.assoc n prims , true

  let add x t =
    let x' = Ident.make x Ident.Node in
    Hashtbl.replace nodes x (x', t);
    x'

  let save () = Hashtbl.fold (fun key (_,ty) env -> (key,ty)::env) nodes []
end

type io = Vinput | Vpatt
module Gamma = struct

  type t = (Ident.t * base_ty * io) M.t

  let empty = M.empty

  let add loc env x t io =
    if M.mem x env then error loc (Clash x);
    let x' = Ident.make x Ident.Stream in
    M.add x (x',t,io) env

  let adds loc io =
    List.fold_left (fun env (x,t) -> add loc env x t io)

  let find loc env x = try
    M.find x env
  with Not_found ->  error loc (UnboundVar x)

  let patts_vars env =
    M.fold (fun _ (x,_,io) s -> if io=Vpatt then S.add x s else s) env S.empty

end

let base_ty_of_ty loc t =
  match t with
  | [t'] -> t'
  | _ -> error loc (ExpectedBase t)



let compatible_base actual_ty expected_ty =
  actual_ty = expected_ty

let compatible actual_ty expected_ty =
  try
    List.fold_left2
      (fun well_t ac_t ex_t ->
	let well_t' = compatible_base ac_t ex_t in
	(well_t && well_t'))
      true actual_ty expected_ty
  with Invalid_argument _ -> false


let real_expr_of_expr te =
  match te.texpr_type with
  | [Treal] -> te
  | [Tint] ->
      { texpr_desc = TE_prim (real_of_int,[te]);
	texpr_type = [Treal];
	texpr_loc = (Lexing.dummy_pos,Lexing.dummy_pos);
      }
  | _ -> assert false

let real_op_of_int_op op =
  match op with
  | Op_add -> Op_add_f
  | Op_sub -> Op_sub_f
  | Op_mul -> Op_mul_f
  | Op_div -> Op_div_f
  | _ -> op

let not_a_nested_tuple e loc =
  match e with
    | PE_tuple el ->
	List.iter
	  (fun e ->
	     match e.pexpr_desc with
		 PE_tuple _ -> error loc FlatTuple;
	       | _ -> ()) el
    | _ -> assert false

let rec is_constant env e =
  match e.texpr_desc with
  | TE_const _ -> true
  | TE_tuple el -> List.for_all (is_constant env) el
  | _ -> false

let rec const_of_expr e =
  match e.texpr_desc with
  | TE_const c -> [c]
  | TE_tuple el ->
      List.fold_right (fun e acc -> const_of_expr e @ acc) el []
  | _ -> assert false

let type_constant = function
  | Cbool _ -> [Tbool]
  | Cint _ -> [Tint]
  | Creal _ -> [Treal]

let rec type_expr env e =
  let desc,t = type_expr_desc env e.pexpr_loc e.pexpr_desc in
  { texpr_desc = desc; texpr_type = t; texpr_loc = e.pexpr_loc; }

and type_expr_desc env loc = function
  | PE_const c ->
      TE_const c , type_constant c

  | PE_ident x ->
      let x, ty, _ = Gamma.find loc env x in
      TE_ident x , [ty]

  | PE_op (Op_not, [e]) ->
      let tt = [Tbool] in
      let te = expected_type env e tt in
      TE_op (Op_not, [te]) , tt

  | PE_op (Op_sub, [e]) ->
      let te = type_expr env e in
      begin match te.texpr_type with
      | [Tint] -> TE_op (Op_sub, [te]) , [Tint]
      | [Treal] -> TE_op (Op_sub_f, [te]) , [Treal]
      | ty -> error e.pexpr_loc (ExpectedNum (ty))
      end

  | PE_op (Op_sub_f, [e]) ->
      let tt = [Treal] in
      let te = expected_type env e tt in
      TE_op (Op_sub_f, [te]) , tt

  | PE_op ((Op_and | Op_or | Op_impl as op), [e1; e2]) ->
      let tt = [Tbool] in
      let te1 = expected_type env e1 tt in
      let te2 = expected_type env e2 tt in
      TE_op (op, [te1; te2]) , tt

  | PE_op ((Op_add | Op_sub | Op_mul | Op_div as op), [e1; e2]) ->
      let te1 = type_expr env e1 in
      let te2 = type_expr env e2 in
      begin match te1.texpr_type, te2.texpr_type with
      | [Tint], [Tint] ->
	  TE_op (op, [te1; te2]), [Tint]
      | [(Tint | Treal)], [(Tint | Treal)] ->
	  TE_op(real_op_of_int_op op, [ real_expr_of_expr te1 ;
		                           real_expr_of_expr te2 ]),
	  [Treal]
      | [(Tint | Treal)], ty -> error e2.pexpr_loc (ExpectedNum (ty))
      | ty, _ -> error e1.pexpr_loc (ExpectedNum (ty))
      end

  | PE_op (Op_mod, [e1; e2]) ->
      let tt = [Tint] in
      let te1 = expected_type env e1 tt in
      let te2 = expected_type env e2 tt in
      TE_op(Op_mod, [te1; te2]) , tt

  | PE_op ((Op_div_f | Op_mul_f | Op_sub_f | Op_add_f as op), [e1; e2]) ->
      let tt = [Treal] in
      let te1 = expected_type env e1 tt in
      let te2 = expected_type env e2 tt in
      TE_op (op, [te1; te2]), tt

  | PE_op (Op_eq | Op_neq as op, [e1; e2]) ->
      let te1 = type_expr env e1 in
      let ty1 = te1.texpr_type in
      let te2 = type_expr env e2 in
      let ty2 = te2.texpr_type in
      begin match ty1, ty2 with
      | [t1], [t2] when t1 = t2 ->
	  TE_op (op, [te1; te2]), [Tbool]
      | _ ->
	  error loc (Other "invalid operands to equality")
      end

  | PE_op (Op_lt | Op_le | Op_gt | Op_ge as op, [e1; e2]) ->
      let te1 = type_expr env e1 in
      let ty1 = te1.texpr_type in
      let te2 = type_expr env e2 in
      let ty2 = te2.texpr_type in
      begin match ty1, ty2 with
      | [Tint], [Tint]
      | [Treal], [Treal] ->
	  TE_op (op, [te1; te2]), [Tbool]
      | _ ->
	  error loc (Other "invalid operands to comparison")
      end

  | PE_op (Op_if, [e1; e2; e3]) ->
      let te1 = expected_type env e1 ([Tbool]) in
      let te2 = type_expr env e2 in
      let te3 = type_expr env e3 in
      let well_typed = compatible te2.texpr_type te3.texpr_type in
      if well_typed then
	let tt = te2.texpr_type in
	TE_op(Op_if, [te1; te2; te3]), tt
      else
	error loc (ExpectedType (te3.texpr_type, te2.texpr_type))

  | PE_op ((Op_eq | Op_neq | Op_lt | Op_le | Op_gt | Op_ge
            | Op_add | Op_sub | Op_mul | Op_div | Op_mod
            | Op_add_f | Op_sub_f | Op_mul_f | Op_div_f
            | Op_not
            | Op_and | Op_or | Op_impl
            | Op_if), []) -> error loc TooFewArguments

  | PE_op ((Op_eq | Op_neq | Op_lt | Op_le | Op_gt | Op_ge
            | Op_add | Op_mul | Op_div | Op_mod
            | Op_add_f | Op_mul_f | Op_div_f
            | Op_and | Op_or | Op_impl
            | Op_if), [ _ ]) -> error loc TooFewArguments

  | PE_op (Op_not, [ _; _ ]) -> error loc TooManyArguments

  | PE_op (Op_if, [ _; _ ]) -> error loc TooFewArguments

  | PE_op ((Op_eq | Op_neq | Op_lt | Op_le | Op_gt | Op_ge
            | Op_add | Op_sub | Op_mul | Op_div | Op_mod
            | Op_add_f | Op_sub_f | Op_mul_f | Op_div_f
            | Op_not
            | Op_and | Op_or | Op_impl
            | Op_if), _) -> error loc TooManyArguments

  | PE_app (f, el) ->
      begin try
	let (f, (t_in,t_out)) , is_prim = Delta.find f in
	let tel = type_args env loc t_in el in
	let app_node = if is_prim then TE_prim(f, tel) else TE_app(f, tel) in
	app_node ,
	begin match t_out with
	| [] -> assert false
	| _ -> t_out
	end
      with Not_found ->
	error loc (UnboundNode f)
      end

  | PE_arrow (e1, e2) ->
      let te1 = type_expr env e1 in
      let ty1 = te1.texpr_type in
      let te2 = type_expr env e2 in
      let ty2 = te2.texpr_type in
      let well_typed = compatible ty1 ty2 in
      if well_typed then
	TE_arrow (te1, te2), ty2
      else error te2.texpr_loc (ExpectedType (ty2, ty1))

  | PE_pre e ->
      let te = type_expr env e in
      TE_pre te, te.texpr_type

  | PE_tuple el as n ->
      not_a_nested_tuple n loc;
      let tel = List.map (type_expr env) el in
      TE_tuple tel,
      (List.map (fun e -> base_ty_of_ty e.texpr_loc e.texpr_type) tel)

and type_args env loc params_ty el =
  let tel = List.map (type_expr env) el in
  let actual_types =
    List.rev
      begin
	List.fold_left
	  (fun res te -> List.rev_append te.texpr_type res)
	  [] tel
      end
  in
  let well_typed =
    compatible actual_types params_ty
  in
  if well_typed then tel
  else error loc (ExpectedType (actual_types, params_ty));


and expected_type env e tt =
  let te = type_expr env e in
  let ty = te.texpr_type in
  if ty = tt then te
  else error e.pexpr_loc (ExpectedType (ty, tt))

and expected_base_type env e =
  let te = type_expr env e in
  match te.texpr_type with
  | [_] -> te
  |  _ ->  error e.pexpr_loc (ExpectedBase (te.texpr_type))

let rec type_patt env p =
  let desc, t = type_patt_desc env p.ppatt_loc p.ppatt_desc in
  { tpatt_desc = desc; tpatt_type = t; tpatt_loc = p.ppatt_loc; }

and type_patt_desc env loc patt =
  match patt with
  | PP_ident x -> begin
      let x, ty =
	match Gamma.find loc env x with
	| x, t, Vpatt -> x, t
	| _  -> error loc (InputVar x)
      in
      [x], [ty]
    end
  | PP_tuple pl ->
      let pl_tyl =
	List.map
	  (fun x ->
	     match Gamma.find loc env x with
	     | x, ty, Vpatt -> x, ty
	     | _  -> error loc (InputVar x)
	  ) pl
      in
      List.split pl_tyl


let type_equation env eq =
  let patt = type_patt env eq.peq_patt in
  let expr = type_expr env eq.peq_expr in
  let well_typed = compatible expr.texpr_type patt.tpatt_type in
  if well_typed then
    { teq_patt = patt; teq_expr = expr; }
  else
    error
      eq.peq_expr.pexpr_loc (ExpectedType (expr.texpr_type, patt.tpatt_type))


let add_vars_of_patt loc s {teq_patt = {tpatt_desc=p}} =
  let add x s =
    if S.mem x s then error loc (Clash x.Ident.name);
    S.add x s
  in
  List.fold_left (fun s x -> add x s) s p

let check_outputs loc env equs =
  let s = List.fold_left (add_vars_of_patt loc) S.empty equs in
  let not_defined = S.diff (Gamma.patts_vars env) s in
  if not (S.is_empty not_defined) then
    error loc (UndefinedOutputs
                 (List.map (fun x -> x.Ident.name) (S.elements not_defined)))

let check_causality loc inputs equs =
  begin try ignore (Scheduling.schedule_equs inputs equs)
  with Scheduling.Causality -> error loc Causality
  end

let type_node n =
  let env = Gamma.adds n.pn_loc Vpatt Gamma.empty (n.pn_output@n.pn_local) in
  let env = Gamma.adds n.pn_loc Vinput env n.pn_input in
  let equs = List.map (type_equation env) n.pn_equs in
  check_outputs n.pn_loc env equs;
  let t_in = List.map (fun (_, ty) -> ty) n.pn_input in
  let t_out = List.map (fun (_, ty) -> ty) n.pn_output in
  let name = Delta.add n.pn_name (t_in,t_out) in
  let input =
    List.map
      (fun (x, ty) -> let x', _, _ = Gamma.find n.pn_loc env x in (x', ty))
      n.pn_input
  in
  let output =
    List.map
      (fun (x, ty) -> let x', _, _ = Gamma.find n.pn_loc env x in (x', ty))
      n.pn_output
  in
  let local =
    List.map
      (fun (x, ty) -> let x', _, _ = Gamma.find  n.pn_loc env x in (x', ty))
      n.pn_local
  in
  let node =
    { tn_name = name;
      tn_input = input;
      tn_output = output;
      tn_local = local;
      tn_equs = equs;
      tn_loc = n.pn_loc; }
  in
  check_causality node.tn_loc node.tn_input equs;
  node

let check_main ft main =
  let (_, ty), is_prim =
    try Delta.find main with Not_found -> error dummy_loc (UnboundNode main)
  in
  match ty, is_prim with
  | (_, [Tbool]), false -> ()
  | (t_in, t_out), false ->
      let n = List.find (fun n -> n.tn_name.Ident.name = main) (List.rev ft) in
      error n.tn_loc (BadMain (t_in, t_out))
  | _ -> errors dummy_loc "The main node cannot be a primitive function"

let type_file f main =
  let ft = List.map type_node f in
  if main <> "" then check_main ft main;
  ft
