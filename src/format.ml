open Ast

type formatted = Line of string | Multiline of string list

let flatten a b =
  match (a, b) with
  | Line a, Line b -> Multiline [a; b]
  | Line a, Multiline b -> Multiline (a :: b)
  | Multiline a, Line b -> Multiline (a @ [b])
  | Multiline a, Multiline b -> Multiline (a @ b)

let make_indent amt = String.make amt ' '

let render = function
  | Line s -> s
  | Multiline lines ->
     List.fold_left (fun acc next -> acc ^ "\n" ^ next) (List.hd lines) (List.tl lines)

let expand_fun f =
  let rec expand f memo =
    match f with
    | {expr = Fun (head, body); _ } -> expand body (head :: memo)
    | bottom -> (List.rev memo, bottom)
  in
  expand f []

(* Function/binding headers are formatted separately from the body since there
   are two ways they may go multi-line:
   1. If the header (arguments, and name in the case of bindings) is too big
      to fit on one line.  In this case the arguments are distributed over
      multiple lines, and the arguments' first characters are aligned in the
      same column.
   2. If the body is too big to fit on the same line as the header.  In this
      case the body is placed on the line following the header, indented 2 more
      spaces.

   The implementation is disatisfying to me currently since it just tries to fit
   the header on a single line, and falls back to multi-line if it doesn't work
   out.  `single_line` and `multi_line` functions inside `format_fun_header` are
   basically just thunks to defer computation of multi-line headers until we
   know it's required.
 *)
let rec format_fun_header ?prefix:(prefix="fun ") ?sep:(sep="->") args indent rem_width =
  let single_line () =
    let buf = Buffer.create 100 in
    Buffer.add_string buf (make_indent indent);
    Buffer.add_string buf prefix;
    let rem_width' = rem_width - indent - (String.length prefix) in
    
    let f rem next =
      match indented_format next 0 rem with
      | (rem', Line next_s) ->
         Buffer.add_string buf next_s;
         Buffer.add_string buf " ";
         rem' - 1
      | _ -> failwith "Multiline format in single-line function header."
    in
    
    let rem = List.fold_left f rem_width' args in
    Buffer.add_string buf sep;
    (rem, Line (Bytes.to_string (Buffer.to_bytes buf)))
  in
  let multi_line () =
    let init = [(make_indent indent) ^ prefix ^ (snd (format (List.hd args) rem_width))] in
    let indent_amt = (indent + String.length prefix) in
    let form x =
      match indented_format x indent_amt rem_width with
      | (_, Line l) -> l
      | _ -> failwith "Can't support multi-line function arguments yet."
    in
    let rec folder xs memo =
      match xs with
      | h :: t -> folder t ((form h) :: memo)
      | [] ->
         let last_with_sep = (List.hd memo) ^ " " ^ sep in
         List.rev (last_with_sep :: (List.tl memo))
    in
    let buf = folder (List.tl args) init in
    (rem_width - 2, Multiline buf)
  in
  let (rem, _res) as res = single_line () in
  if rem > 0 then
    res
  else
    multi_line ()

(* Pieces together a function header and body.  `prefix` and `sep` are
   overridden for `let` bindings, with the default values set appropriately for
   function literals.
 *)
and format_fun ?prefix:(prefix="fun ") ?sep:(sep="->") args body_ast indent rem_width =
  let (rem_header, header) = format_fun_header ~prefix ~sep args indent rem_width in
  let (rem_body, body) = indented_format body_ast indent rem_header in
  if rem_body <= 0 then
    begin
      let (rem, body) = indented_format body_ast (indent + 2) (rem_width - 2) in
      (rem, flatten header body)
    end
  else
    begin
      match header, body with
      | Line _, Line _ when rem_header > rem_body ->
         let full = (render header) ^ " " ^ (render body) in
         (rem_width - indent - String.length full, Line full)
      (* For now we give up and indent the body on the next line.  This could
         probably be improved, aesthetically.
       *)
      | _, _ ->
         let (rem_body, body) = indented_format body_ast (indent + 2) (rem_width - 2) in
         (rem_body, flatten header body)
    end

(* Main entry point for this module.

   This implementation is not terribly efficient because it generally tries
   to format things as a single line, falling back to multi-line if the single
   line rendering exceeds the available width.
 *)
and format expr rem_width =
  let rem, lines = indented_format expr 0 rem_width in
  (rem, render lines)
    
(* Remaining space (`rem_width`) and left margin (`indent`) separately tracked.

   How about indent and max width instead?  Otherwise maybe can't recover full
   intended width while recursing/unwinding.
 *)
and indented_format expr indent rem_width =
  match expr with
  | { expr = Int i; _ } ->
     let si = string_of_int i in
     (rem_width - (String.length si), Line ((make_indent indent) ^ si))

  | { expr = Label name; _ } ->
     let l = name in
     (rem_width - (String.length l), Line ((make_indent indent) ^l))

  (* Function literals, handled separately from functions bound to names because
     bound functions have slightly more complicated "prefix" values.
   *)
  | { expr = Fun _ ; _ } as f ->
     let (sugared_args, body) = expand_fun f in
     format_fun sugared_args body indent rem_width

  (* Bindings for functions.  *)
  | { expr = Binding { name; expr = { expr = Fun _ ; _} as expr; _}; _ } ->
     let args, body = expand_fun expr in
     let let_prefix = "let " in
     let (_, name) = format name (rem_width - (String.length let_prefix)) in
     let prefix = (String.make indent ' ') ^ let_prefix ^ name ^ " " in
     format_fun ~prefix:prefix ~sep:"=" args body indent rem_width

  (* Immutable variable bindings.  *)
  | { expr = Binding { body = None; _ }; _ } ->
     failwith "No variable binding support."

  | { expr = Binding { body = Some _body; _ }; _ } ->
     failwith "No local binding support."

  | _ ->
     failwith "format not implemented"
