(*

Source here has been a big help:
https://github.com/jorisgio/menhir-workshop/blob/master/example.ml

 *)

module Ast = Ast
module Format = Format

let module_of_str src =
  Bf_parser.module_of_string src

let expr_of_str src =
  Bf_parser.expr_of_string src
