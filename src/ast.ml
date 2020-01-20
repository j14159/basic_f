type src = { file : string
           ; line : int
           ; col : int
           }

(* TODO:  unicode.  *)
type text = string

type expr =
  | Int of int
  | Float of float
  | String of text
  | Label of string
  | Fun of node * node
  | App of node * node
  | Binding of { name : node; expr : node; body : node option }

and node = { expr : expr; src : src }

let expr_printer = function
  | Int i -> "Int " ^ string_of_int i
  | Label l -> "Label " ^ l
  | _ -> "Printer not implemented."
