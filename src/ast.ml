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
  | App of node * (node list)
  | Binding of { name : node; expr : node; body : node option }

and node = { expr : expr; src : src }

let expr_printer = function
  | Int i -> "Int " ^ string_of_int i
  | Label l -> "Label " ^ l
  | _ -> "Printer not implemented."

let string_of_src = function
  | { file; line; col } -> "@[" ^ file
                           ^ ", l" ^ string_of_int line
                           ^ ", c" ^ string_of_int col
                           ^ "]"

let rec string_of_node = function
  | { src; expr = Label l } ->
     "Label " ^ l ^ " " ^ (string_of_src src)
  | { src; expr = Int i } ->
     "Int " ^ (string_of_int i) ^ " " ^ (string_of_src src)
  | { src; expr = App (n, args) } ->
     "App" ^ (string_of_src src)
     ^ (string_of_node n) ^ " "
     ^ (List.fold_left (fun acc next -> acc ^ " " ^ string_of_node next) "" args)
  | _ -> "string_of_expr not implemented."

let label text file line col =
  { expr = Label text; src = { file; line; col } }
