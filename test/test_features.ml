
let%spec "test features" =
  let e = 1 in
  (e = 1) [@true "equal"];
  (e = 2) [@false "not equal"]

let%spec "some specs" =
  1 + 1 [@eq 2];
  1 + 0 [@ne  2 [@cmp ]]

(* open OUnit2  *)

(* let __fun_1 _ = *)
(*   let e = 1 in *)
(*   assert_bool e *)

(* let suite = "suite" >::: [ *)
(*   "test features" >:: __fun_1 *)
(* ] *)

(* let () = *)
(*   run_test_tt_main suite *)

