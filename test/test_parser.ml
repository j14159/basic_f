open OUnit2

open Basic_f.Ast
open Lexing (* Used for a printer used by test_syntax_error.  *)
   
let test_fun_parsing _ =
  let src = "let f = fun a -> a" in
  match Basic_f.parse_str src with
  | Result.Ok [{ expr = Binding (name, body); src = binding_src }] ->
     assert_equal binding_src { line = 0; col = 0; file = "" };
     assert_equal name { expr = Label "f" ; src = { file = ""; line = 0; col = 4 } };
     let expected_body = { expr = Fun ( { expr = Label "a"
                                        ; src = { file = ""
                                                ; line = 0
                                                ; col = 12
                                                }
                                        }
                                      , { expr = Label "a"
                                        ; src = { file = ""
                                                ; line = 0
                                                ; col = 17
                                                }
                                        }
                                    )
                         ; src = { file = ""
                                 ; line = 0
                                 ; col = 12
                                 }
                         }
     in
     assert_equal body expected_body
  | Result.Error _ ->
     failwith "Error parsing a simple function binding."
  | _ ->
     failwith "Expected binding, got something else."

let test_double_binding_parsing _ =
  let src = "let id = fun x -> x let y = 2" in
  match Basic_f.parse_str src with
  | Result.Error _ -> failwith "Failed to parse bindings."
  | Result.Ok [ { expr = Binding ({ expr = Label name1; _ }, _); _ }
              ; { expr = Binding ({ expr = Label name2; _ }, _); _ } ]
    ->
     assert_equal name1 "id";
     assert_equal name2 "y"
  | Result.Ok _ -> failwith "Failed to parse exactly two bindings."

let test_binding_desugar _ =
  let src = "let id x = x" in
  match Basic_f.parse_str src with
  | Result.Error _ -> failwith "Failed to parse sugared binding."
  | Result.Ok [ {expr = Binding ({ expr = Label name; _ }, body); src } ] ->
     assert_equal name "id";
     assert_equal src { file = ""; line = 0; col = 0 };
     begin
       match body with
       | { expr = Fun ({ expr = Label arg; _ }, { expr = Label var; _ }); _ } ->
          assert_equal var arg;
          assert_equal var "x"
       | _ ->
          failwith "Did not get function from binding."
     end
  | _ -> failwith "Failed to parse a single binding."
       
let lexing_pos_to_str { pos_fname; pos_lnum; pos_bol; pos_cnum } =
  Printf.sprintf "{ %s %d %d %d }" pos_fname pos_lnum pos_bol pos_cnum
                 
let test_syntax_error _ =
  let src = "let id let" in
  let res = Basic_f.parse_str src in
  assert (Result.is_error res);
  let printer (a, b) = Printf.sprintf "(%s, %s)" (lexing_pos_to_str a) (lexing_pos_to_str b) in
  (* Parser seems to report position on previous "good" token, bears investigation:  *)
  let expected = ( { pos_fname = ""; pos_lnum = 0; pos_bol = 0; pos_cnum = 4 }
                 , { pos_fname = ""; pos_lnum = 0; pos_bol = 0; pos_cnum = 6 }
                 )
  in
  assert_equal expected (Result.get_error res) ~printer:printer
  
let suite =
  "Parser test suite" >:::
    [ "Simple function parsing" >:: test_fun_parsing
    ; "Multiple binding parsing" >:: test_double_binding_parsing
    ; "Binding desugar" >:: test_binding_desugar
    ; "Simple syntax error" >:: test_syntax_error
    ]
   
let _ =
  run_test_tt_main suite
