(*

Source here has been a big help:
https://github.com/jorisgio/menhir-workshop/blob/master/example.ml

 *)

module Ast = Ast
module Format = Format

open Bf_parser

let parse_str src =
  let rec f n memo =
    match n () with
    | Next expr -> f n (expr :: memo)
    | End -> Result.ok (List.rev memo)
    | Fail (pos1, pos2) -> Result.error (pos1, pos2)
  in
  let iter = iter_of_str src in
  f iter []
