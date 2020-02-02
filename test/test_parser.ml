open OUnit2

open Basic_f.Ast
open Lexing (* Used for a printer used by test_syntax_error.  *)

(** Helper to give a failure message with formatted node.  *)
let node_reason base unexpected_node =
  base ^ (snd @@ Basic_f.Format.format unexpected_node 80)

(** Simpler string equality assert with the right printer.  *)
let str_eq = assert_equal ~printer:(fun x -> x)

let test_fun_parsing _ =
  let src = "let f = fun a -> a" in
  match Basic_f.module_of_str src with
  | Result.Ok [{ expr = Binding { name; expr; _ }; src = binding_src }] ->
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
     assert_equal expr expected_body
  | Result.Error _ ->
     failwith "Error parsing a simple function binding."
  | _ ->
     failwith "Expected binding, got something else."

let test_double_binding_parsing _ =
  let src = "let id = fun x -> x let y = 2" in
  match Basic_f.module_of_str src with
  | Result.Error _ -> failwith "Failed to parse bindings."
  | Result.Ok [ { expr = Binding { name = { expr = Label name1; _ }; _}; _ }
              ; { expr = Binding { name = { expr = Label name2; _ }; _}; _ } ]
    ->
     assert_equal name1 "id";
     assert_equal name2 "y"
  | Result.Ok _ -> failwith "Failed to parse exactly two bindings."

let test_binding_desugar _ =
  let src = "let id x = x" in
  match Basic_f.module_of_str src with
  | Result.Error _ -> failwith "Failed to parse sugared binding."
  | Result.Ok [ {expr = Binding { name = { expr = Label name; _ }; expr; _ }; src } ] ->
     assert_equal name "id";
     assert_equal src { file = ""; line = 0; col = 0 };
     begin
       match expr with
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
  let res = Basic_f.module_of_str src in
  assert (Result.is_error res);
  let printer (a, b) = Printf.sprintf "(%s, %s)" (lexing_pos_to_str a) (lexing_pos_to_str b) in
  (* Parser seems to report position on previous "good" token, bears investigation:  *)
  let expected = ( { pos_fname = ""; pos_lnum = 0; pos_bol = 0; pos_cnum = 4 }
                 , { pos_fname = ""; pos_lnum = 0; pos_bol = 0; pos_cnum = 6 }
                 )
  in
  assert_equal expected (Result.get_error res) ~printer:printer
  
let test_local_binding _ =
  let src = "let x = let y = 2 in y" in
  match Basic_f.module_of_str src with
  | Result.Ok [{ expr = Binding { name = { expr = Label name; _ };  expr = b; _ }; _ }] ->
     assert_equal "x" name;
     begin
       match b with
       | { expr = Binding { name = { expr = Label local_name; _ }
                          ; expr = { expr = e; _ }
                          ; body = Some { expr = body; _ } }
         ; _ }
         ->
          assert_equal "y" local_name;
          assert_equal (Int 2) e ~printer:expr_printer;
          assert_equal (Label "y") body ~printer:expr_printer
       | _ ->
          failwith "Incorrect binding parsed."
     end
  | _ ->
     failwith "Failed to parse a top-level binding."

let test_simple_apply _ =
  let src = "id 1" in
  match Result.get_ok (Basic_f.expr_of_str src) with
  | Some { src = app_src; expr = App ({ expr = Label f; _ }, [{expr = Int arg; _ }]) } ->
     assert_equal 0 app_src.line;
     assert_equal 0 app_src.col;
     str_eq "id" f;
     assert_equal 1 arg ~printer:string_of_int
  | Some _ -> failwith "Expected application, parsed something else."
  | None -> failwith "Expected application, got None."

let get_bound_expr = function
  | { expr = Binding { expr = e; _ }; _ } -> e
  | _ -> failwith "Not a binding."

let test_apply_in_local _ =
  let src = "let f x = let g a b = h a b in g x 1" in
  match get_bound_expr (List.hd (Result.get_ok (Basic_f.module_of_str src))) with
  | {expr = Fun (_, { expr = Binding { expr = e; body = Some b; _ }; _ }); _ } ->
     begin match e with
     | { expr = Fun ({ expr = Label arg_a; _ },
                     { expr = Fun (_,
                                   { expr = App ({ expr = Label h; _ },
                                                 [{ expr = Label a; _ }
                                                 ; { expr = Label b; _ }
                                                 ]
                                              )
                                   ; _
                                   }
                                )
                     ; _}
                  )
       ; _ } ->
        str_eq "h" h;
        str_eq "a" a;
        str_eq a arg_a;
        str_eq "b" b
     | nfa -> failwith @@ node_reason "Not a function application:  " nfa
     end;
     begin match b with
     | { expr = App (f, args); _ } ->
        let expected_f = { src = { file = ""; line = 0; col = 31 }
                         ; expr = Label "g"
                         }
        in
        assert_equal expected_f f ~printer:Basic_f.Ast.string_of_node;
        let expected_arg1 = { src = { file = ""; line = 0; col = 33 }
                            ; expr = Label "x"
                            }
        in
        let expected_arg2 = { src = { file = ""; line = 0; col = 35 }
                            ; expr = Int 1
                            }
        in
        begin match args with
        | [a1; a2] ->
           assert_equal expected_arg1 a1 ~printer:Basic_f.Ast.string_of_node;
           assert_equal expected_arg2 a2 ~printer:Basic_f.Ast.string_of_node;
        | l -> failwith ("Expected 2 args, got " ^ (string_of_int (List.length l)))
        end
     | _ -> failwith "Body does not contain a function application."
     end
  | other ->
     failwith @@ node_reason "Didn't get an internal binding, got:  " other

let test_nested_apply _ =
  let src = "f (g x)" in
  match Option.get (Result.get_ok (Basic_f.expr_of_str src)) with
  | { expr = App (n, [a]); _ } ->
     let expected_n = { expr = Label "f"; src = { file = ""; line = 0; col = 0 } } in
     assert_equal expected_n n ~printer:string_of_node;
     let expected_arg = { expr = App (label "g" "" 0 3, [label "x" "" 0 5])
                        ; src = { file = ""; line = 0; col = 3 }
                        }
     in
     assert_equal expected_arg a ~printer:string_of_node
  | other ->
     failwith @@ node_reason "Expected application but got " other

let suite =
  "Parser test suite" >:::
    [ "Simple function parsing" >:: test_fun_parsing
    ; "Multiple binding parsing" >:: test_double_binding_parsing
    ; "Binding desugar" >:: test_binding_desugar
    ; "Simple syntax error" >:: test_syntax_error
    ; "Basic local binding" >:: test_local_binding
    ; "Simple application" >:: test_simple_apply
    ; "Apply a locally bound function" >:: test_apply_in_local
    ; "Nested application/term" >:: test_nested_apply
    ]
   
let _ =
  run_test_tt_main suite
