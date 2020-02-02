%token <int> INT
%token <float> FLT
%token <string> STR
%token <string> LBL
%token LET
%token ASSIGN
%token IN
%token ARROW
%token FUN
%token EOF

%{
    open Lexing
    open Ast

    let pos expr p =
      let { pos_fname; pos_lnum; pos_bol; pos_cnum } = p in
      { expr; src = { file = pos_fname; line = pos_lnum; col = pos_cnum - pos_bol } }

    let desugar_fun_args args =
      let rec f xs =
	match xs with
	| [x] -> x
	| { src; _ } as h :: t -> { expr = Ast.Fun (h, f t); src }
	| _ -> failwith "Unreachable, no fun args."
      in
      f args
  %}

%start <Ast.node list> root_expr single_expr

%%


root_expr:
  | EOF { [] }
  | bindings = nonempty_list(top_binding) EOF { bindings }
;

single_expr:
  | e = expr; EOF { [e] }

expr:
  | v = value { v }
  | l = label { l }
  | l = local_binding { l }
;

value:
  | i = INT { pos (Ast.Int i) $startpos }
  | f = FLT { pos (Ast.Float f) $startpos }
  | s = STR { pos (Ast.String s) $startpos }
  | f = fn { f }
;

top_binding:
  | LET; header = nonempty_list(label); ASSIGN ; e = expr
    { match header with
      | [name] ->
	 pos (Binding { name; expr = e; body = None }) $symbolstartpos
      | name :: args ->
	 pos (Binding { name; expr = desugar_fun_args (args @ [e]); body = None }) $symbolstartpos
      | _ ->
	 failwith "Unreachable no-arg and no-name binding case"    
    }
;

local_binding:
  | LET; header = nonempty_list(label); ASSIGN ; e = expr; IN; bind_in = expr
    { match header with
      | [name] ->
	 pos (Binding { name; expr = e; body = Some bind_in }) $symbolstartpos
      | name :: args ->
	 pos (Binding { name
		      ; expr = desugar_fun_args (args @ [e])
		      ; body = Some bind_in
	     })
	 $symbolstartpos
      | _ ->
	 failwith "Unreachable no-arg and no-name binding case"
    }

(* e.g.
    fn x y z -> add x (add y z)
 *)
fn:
  | FUN; args = nonempty_list(label); ARROW; e = expr
    { desugar_fun_args (args @ [e]) }
;

label:
  | l = LBL { pos (Ast.Label l) $startpos }
;
