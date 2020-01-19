module I = Bf_parser_gen.MenhirInterpreter

let rec parse next_token checkpoint =
  match checkpoint with
  | I.InputNeeded _env ->
     let t = next_token () in
     let checkpoint' = I.offer checkpoint t in
     parse next_token checkpoint'
  | I.Accepted expr -> Result.ok expr
  | I.Rejected -> failwith "Can't reject, need syntax error."
  | I.HandlingError env ->
     begin
       match I.top env with
       | None -> failwith "Need a position for an error."
       (* This is actually misleading since the reported positions are for the
          element *prior* to the location of the error.

          TODO: fix it, dig into Menhir more.
        *)
       | Some (I.Element (_, _, p1, p2)) -> Result.error (p1, p2)
     end
  | I.Shifting _ | I.AboutToReduce _ -> parse next_token (I.resume checkpoint)

type 'a parse_result = Next of 'a | End | Fail of Lexing.position * Lexing.position

(* Bad version of an iterator. *)
let next lexer start_pos =
  fun () ->
  match parse lexer start_pos with
  | Result.Ok (Some x) -> Next x
  | Result.Ok None -> End
  | Result.Error (p1, p2) -> Fail (p1, p2)      (* TODO real errors *)
       
let iter_of_str str =
  let buf = Sedlexing.Utf8.from_string str in
  let lexer = Bf_lexer.lexer buf in
  let (init_pos, _) = Sedlexing.lexing_positions buf in
  next lexer (Bf_parser_gen.Incremental.root_expr init_pos)
              

