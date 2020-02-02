(*

Helpful links for Menhir's incremental API:

https://discuss.ocaml.org/t/example-of-menhir-sedlex-parser-using-the-error-states-generation/4302
https://github.com/jorisgio/menhir-workshop
 *)

open Bf_parser_gen

let alphanumeric = [%sedlex.regexp? alphabetic | '0' .. '9' | '_']

let label = [%sedlex.regexp? (lowercase | other_lowercase | '_') , Star (alphanumeric)]

let label' = [%sedlex.regexp? Plus alphabetic]
   
let rec tokenize buf =
  match%sedlex buf with
  | white_space -> tokenize buf
  | "(" -> OPEN_P
  | ")" -> CLOSE_P
  | "fun" -> FUN
  | "let" -> LET
  | "=" -> ASSIGN
  | "->" -> ARROW
  | "in" -> IN
  (* Just for fun but no test for this yet. *)
  | 2192 -> ARROW
  | label -> LBL (Sedlexing.Utf8.lexeme buf)
  | Plus ('0' .. '9') -> INT (int_of_string (Sedlexing.Utf8.lexeme buf))
  | eof -> EOF
  | _ -> failwith "Unreachable base for Lexer"

let lexer buf =
  Sedlexing.with_tokenizer tokenize buf

let str_lexer str =
  lexer (Sedlexing.Utf8.from_string str)
