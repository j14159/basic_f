(** Very primitive notion of a module currently as "a list of top-level
    bindings."
 *)
val module_of_string : string -> (Ast.node list, (Lexing.position * Lexing.position)) Result.t

val expr_of_string : string -> (Ast.node option, (Lexing.position * Lexing.position)) Result.t
