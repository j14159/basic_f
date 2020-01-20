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

let of_string s expr_f =
  let buf = Sedlexing.Utf8.from_string s in
  let lexer = Bf_lexer.lexer buf in
  let (init_pos, _) = Sedlexing.lexing_positions buf in
  parse lexer (expr_f init_pos)

let module_of_string s =
  of_string s Bf_parser_gen.Incremental.root_expr

let expr_of_string s =
  let res = of_string s Bf_parser_gen.Incremental.single_expr in
  Result.map (function | [] -> None | x :: _ -> Some x ) res
