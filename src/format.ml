open Ast

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
    Buffer.add_string buf prefix;
    let rem_width' = rem_width - (String.length prefix) in
    
    let f rem next =
      let (rem', next_s) = format next rem in
      Buffer.add_string buf next_s;
      Buffer.add_string buf " ";
      rem' - 1
    in
    
    let rem = List.fold_left f rem_width' args in
    Buffer.add_string buf sep;
    (rem, Bytes.to_string (Buffer.to_bytes buf))
  in
  let multi_line () =
    Printf.printf "multi-line";
    let buf = Buffer.create 100 in
    Buffer.add_string buf prefix;
    let indent_str = String.make (indent + (String.length prefix)) ' ' in
    let rec folder xs memo =
      match xs with
      | [] ->
         let f acc next =
           (* TODO:  this is gross, rethink this.  *)
           if not (String.equal "\n" next) then Buffer.add_string acc indent_str;
           Buffer.add_string acc next;
           acc
         in
         let to_fold = List.rev memo in
         Buffer.add_string buf (List.hd to_fold);
         List.fold_left f buf (List.tl to_fold)
      | [last] -> folder [] (((snd (format last rem_width)) ^ " " ^ sep) :: memo)
      | h :: t -> folder t ("\n" :: (snd (format h rem_width)) :: memo)
    in
    let buf = folder args [] in
    (rem_width - 2, Bytes.to_string (Buffer.to_bytes buf))
  in
  let (rem, _) as res = single_line () in
  if rem > 0 then
    res
  else
    multi_line ()
  
(* Pieces together a function header and body.  `prefix` and `sep` are
   overridden for `let` bindings, with the default values set appropriately for
   function literals.
 *)
and format_fun ?prefix:(prefix="fun ") ?sep:(sep="->") args body_ast indent rem_width =
  (* Arbitrarily sized Buffer.  *)
  let buf = Buffer.create 100 in
  let add = Buffer.add_string buf in
  let (rem, header) = format_fun_header ~prefix ~sep args indent rem_width in
  let (rem, body) = format body_ast rem in
  if rem <= 0 then
    begin
      let (rem, body) = indented_format body_ast (indent + 2) (rem_width - 2) in
      add header;
      add "\n";
      add (String.make (indent + 2) ' ');
      add body;
      (rem, Bytes.to_string (Buffer.to_bytes buf))
    end
  else
    begin
      add header;
      add " ";
      add body;
      let full = Bytes.to_string (Buffer.to_bytes buf) in
      (rem_width - indent - String.length full, full)
    end

(* Main entry point for this module.

   This implementation is not terribly efficient because it generally tries
   to format things as a single line, falling back to multi-line if the single
   line rendering exceeds the available width.
 *)
and format expr rem_width =
  indented_format expr 0 rem_width
    
and indented_format expr indent rem_width = 
  match expr with
  | { expr = Int i; _ } ->
     let si = string_of_int i in
     (rem_width - (String.length si), si)

  | { expr = Label name; _ } ->
     (rem_width - (String.length name), name)

  (* Function literals, handled separately from functions bound to names because
     bound functions have slightly more complicated "prefix" values.
   *)
  | { expr = Fun _ ; _ } as f ->
     let (sugared_args, body) = expand_fun f in
     format_fun sugared_args body indent rem_width

  (* Bindings for functions.  *)
  | { expr = Binding {name; expr = { expr = Fun _ ; _} as expr; _}; _ } ->
     let args, body = expand_fun expr in
     let let_prefix = "let " in
     let (_, name) = format name (rem_width - (String.length let_prefix)) in
     let prefix = (String.make indent ' ') ^ let_prefix ^ name ^ " " in
     format_fun ~prefix:prefix ~sep:"=" args body indent rem_width

  (* Immutable variable bindings.  *)
  | { expr = Binding _; _ } ->
     failwith "No variable binding support."

  | _ ->
     failwith "format not implemented"
