%token <int> INT
%token <float> FLT
%token <string> STR
%token <string> LBL
%token LET
%token ASSIGN
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

%start <Ast.node option> root_expr

%%

root_expr:
  | EOF { None }
  | e = expr { Some e }

expr:
  | v = value { v }
  | l = label { l }
  | b = binding { b }
;
 
value:
  | i = INT { pos (Ast.Int i) $startpos }
  | f = FLT { pos (Ast.Float f) $startpos }
  | s = STR { pos (Ast.String s) $startpos }
  | f = fn { f }
;

binding:
  | LET; name = label; ASSIGN; e = expr { pos (Binding (name, e)) $symbolstartpos }
  | LET; header = list(label); ASSIGN ; e = expr
    { match header with
      | [name] ->
	 pos (Binding (name, e)) $symbolstartpos
      | name :: args ->
	 pos (Binding (name, desugar_fun_args (args @ [e]))) $symbolstartpos
      | _ ->
	 failwith "Unreachable no-arg and no-name binding case"    
    }
;

(* e.g.
    fn x y z -> add x (add y z)
 *)
fn:
  | FUN; args = list(label); ARROW; e = expr
    { desugar_fun_args (args @ [e]) }
;

label:
  | l = LBL { pos (Ast.Label l) $startpos }
;
