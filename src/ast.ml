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
  | Binding of node * node

and node = { expr : expr; src : src }


exception Invalid_binding_name of src
        
let binding_name expr =
  match expr with
  | { expr = Binding ({ expr = Label name; _ }, _); _ } ->
     name
  | { expr = Binding ({ src = { file; line; col }; _ }, _); _} ->
     raise (Invalid_binding_name { file; line; col })
  | _ ->
     failwith "Not a binding" (* TODO:  this is not helpful *)
