%{

  open Asttypes
  open Parse_ast

  let loc () = symbol_start_pos (), symbol_end_pos ()
  let mk_expr e = { pexpr_desc = e; pexpr_loc = loc () }
  let mk_patt p = { ppatt_desc = p; ppatt_loc = loc () }

%}

%token AND
%token ARROW
%token BOOL
%token CONST
%token COLON
%token COMMA
%token <Asttypes.op> COMP
%token <bool> CONST_BOOL
%token <int> CONST_INT
%token <float> CONST_REAL
%token DIV
%token ELSE
%token END
%token EOF
%token EQUAL
%token NEQ
%token REAL
%token <string> IDENT
%token IF
%token IMPL
%token INT
%token LET
%token LPAREN
%token MINUS
%token MOD
%token NODE
%token NOT
%token OR
%token PLUS
%token PRE
%token RETURNS
%token RPAREN
%token SEMICOL
%token SLASH
%token STAR
%token TEL
%token THEN
%token VAR


%nonassoc THEN
%nonassoc ELSE
%right ARROW
%left IMPL
%left OR
%left AND
%left COMP EQUAL NEQ                          /* < <= > >= <> = <> */
%left PLUS MINUS                              /* + -  */
%left STAR SLASH DIV MOD                      /* * /  mod */
%nonassoc uminus                              /* - */
%nonassoc NOT PRE                             /* not pre */
%left DOT

/* Point d'entrée */

%start file
%type <Parse_ast.p_file> file

%%

file: node_decs EOF { $1 }
;

node_decs:
| /* empty */       { [] }
| node node_decs    { $1 :: $2 }
;


node:
| NODE IDENT LPAREN in_params RPAREN
  RETURNS LPAREN out_params RPAREN SEMICOL
  local_params
  LET eq_list TEL semi_opt
    { { pn_name = $2;
	pn_input = $4;
	pn_output = $8;
	pn_local = $11;
	pn_equs = $13;
	pn_loc = loc(); } }
;

in_params:
| /* empty */
    { [] }
| param_list
    { $1 }
;


out_params:
| param_list
    { $1 }
;

local_params:
| /* empty */
    { [] }
| VAR param_list_semicol
    { $2 }
;

param_list:
| param
    { $1 }
| param SEMICOL param_list
    { $1 @ $3 }
;

param_list_semicol:
| param  SEMICOL
    { $1 }
| param SEMICOL param_list_semicol
    { $1 @ $3 }
;


param:
  | ident_comma_list COLON typ
      { let typ = $3 in
        List.map (fun id -> (id, typ)) $1 }
;

eq_list:
| eq
    { [$1] }
| eq eq_list
    { $1 :: $2 }
;

eq:
| pattern EQUAL expr SEMICOL
    { { peq_patt = $1; peq_expr = $3; } }
;

pattern:
| IDENT
    { mk_patt (PP_ident $1) }
| LPAREN IDENT COMMA ident_comma_list RPAREN
    { mk_patt (PP_tuple($2::$4)) }
;

expr:
| LPAREN expr RPAREN
    { $2 }
| const
    { $1 }
| IDENT
    { mk_expr (PE_ident $1)}
| IDENT LPAREN expr_comma_list_empty RPAREN
    { mk_expr (PE_app ($1, $3))}
| IF expr THEN expr ELSE expr
    { mk_expr (PE_op (Op_if, [$2; $4; $6])) }
| expr PLUS expr
    { mk_expr (PE_op (Op_add, [$1; $3])) }
| expr MINUS expr
    { mk_expr (PE_op (Op_sub, [$1; $3])) }
| expr STAR expr
    { mk_expr (PE_op (Op_mul, [$1; $3])) }
| expr SLASH expr
    { mk_expr (PE_op (Op_div, [$1; $3])) }
| expr DIV expr
    { mk_expr (PE_op (Op_div, [$1; $3])) }
| expr MOD expr
    { mk_expr (PE_op (Op_mod, [$1; $3])) }
| expr COMP expr
    { mk_expr (PE_op ($2, [$1; $3])) }
| expr EQUAL expr
    { mk_expr (PE_op (Op_eq, [$1; $3])) }
| expr NEQ expr
    { mk_expr (PE_op (Op_neq, [$1; $3])) }
| expr AND expr
    { mk_expr (PE_op (Op_and, [$1; $3])) }
| expr OR expr
    { mk_expr (PE_op (Op_or, [$1; $3])) }
| expr IMPL expr
    { mk_expr (PE_op (Op_impl, [$1; $3])) }
| expr ARROW expr
    { mk_expr (PE_arrow ($1, $3)) }
| MINUS expr /* %prec uminus */
    { mk_expr (PE_op (Op_sub, [$2])) }
| NOT expr
    { mk_expr (PE_op (Op_not, [$2])) }
| PRE expr
    { mk_expr (PE_pre ($2)) }
| LPAREN expr COMMA expr_comma_list RPAREN
    { mk_expr (PE_tuple ($2::$4)) }
;

const:
| CONST_BOOL
    { mk_expr (PE_const (Cbool $1)) }
| CONST_INT
    { mk_expr (PE_const (Cint $1)) }
| CONST_REAL
    { mk_expr (PE_const (Creal $1)) }
;

ident_comma_list:
| IDENT COMMA ident_comma_list
    { $1 :: $3 }
| IDENT { [$1] }
;

expr_comma_list_empty:
    { [] }
| expr_comma_list { $1 }
;

expr_comma_list:
| expr COMMA expr_comma_list
    { $1 :: $3 }
| expr { [$1] }
;

typ:
| BOOL   { Tbool }
| INT    { Tint }
| REAL   { Treal }
;

semi_opt:
    { () }
| SEMICOL
    { () }
;
