open OUnit2

let mod_of_str src =
  List.hd (Result.get_ok (Basic_f.module_of_str src))

let expr_of_str s =
  Option.get (Result.get_ok (Basic_f.expr_of_str s))
  
(* Make sure that a small function formatted in an 80-column space doesn't
   actually change through the formatter.
 *)
let test_no_change_fun _ =
  let src = "fun x -> x" in
  let ast = expr_of_str src in
  let (rem_len, res) = Basic_f.Format.format ast 80 in
  let _ = assert_equal 70 rem_len ~printer:string_of_int in
  assert_equal src res ~printer:(fun x -> x)

(* A small 10-column function should get split when the width is under 10. *)
let test_fun_with_low_width _ =
  let src = "fun x -> x" in
  let ast = expr_of_str src in

  (* Simulating a simple constraint that _should_ move the body:  *)
  let (rem_len1, res1) = Basic_f.Format.format ast 7 in

  (* Simulating a constraint where only the `fun` keyword fits.  At this point
     the formatter should give up and pack things multi-line without trying to
     move to another line.
   *)
  let (rem_len2, res2) = Basic_f.Format.format ast 3 in
  (* The two format results should be the same since the formatter does *not*
     split the first argument from the `fun` keyword.
   *)

  (* We force the same basic multi-line split with both constraints, so the
     formatted source text should be identical.
   *)
  let p v = "\n" ^ v ^ "\n---" in
  let expected = "fun x ->\n  x" in
  assert_equal res1 res2 ~printer:p;
  assert_equal (String.length expected) (String.length res1) ~printer:string_of_int;
  assert_equal expected res1 ~printer:p;
  assert_equal 4 rem_len1 ~printer:string_of_int;
  assert_equal 0 rem_len2 ~printer:string_of_int

(* Binding a function literal should reformat as sugared binding syntax.  *)
let test_reformat_fun_binding _ =
  let src = "let id = fun x -> x" in
  let ast = mod_of_str src in
  let (rem_cols, formatted) = Basic_f.Format.format ast 80 in
  assert_equal "let id x = x" formatted;
  assert_equal 68 rem_cols ~printer:string_of_int

(* With limited width and more than one function argument, make sure everything
   gets distributed properly across multiple lines.
 *)
let test_distribute_binding_args _ =
  let src = "let f first _second _third = first" in
  let ast = mod_of_str src in
  let expected = "let f first\n      _second\n      _third =\n  first" in
  let (_, res) = Basic_f.Format.format ast 20 in
  assert_equal expected res ~printer:(fun x -> "\n" ^ x)

let test_local_binding_alignment _ =
  let src = "let f x = let y = 1 in let z = 2 in x" in
  let ast = mod_of_str src in
  (* No function application in the parser yet:  *)
  let expected = "let f x =\n  let y = 1 in\n  let z = 2 in\n  x" in
  let _, res = Basic_f.Format.format ast 80 in
  assert_equal expected res ~printer:(fun x -> "\n" ^ x)

(* A function value bound inside another top-level binding should be both
   sugared and aligned after parsing and formatting.
 *)
let test_local_fun_rewriting _ =
  (* Unaligned local bindings here:  *)
  let src = "let x = \n   let id = fun x -> x in\ let y = 2 in id" in
  let ast = mod_of_str src in
  let expected = "let x =\n  let id x = x in\n  let y = 2 in\n  id" in
  let _, res = Basic_f.Format.format ast 800 in
  assert_equal expected res ~printer:(fun x -> "\n" ^ x)
  
let suite =
  "Simple source code formatter tests" >:::
    [ "Formatting a function for no change" >:: test_no_change_fun
    ; "Formatting a function that should split." >:: test_fun_with_low_width
    ; "Formatting a bound function literal" >:: test_reformat_fun_binding
    ; "Multi-line, multiple-arg binding" >:: test_distribute_binding_args
    ; "Local binding alignment" >:: test_local_binding_alignment
    ; "Local binding rewriting" >:: test_local_fun_rewriting
    ]

let _ =
  run_test_tt_main suite
