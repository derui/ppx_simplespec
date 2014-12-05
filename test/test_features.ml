
let%spec "test features" =

  let e = 1 in
  e [@eq 1];
  (e = 1) [@true "" ][@false "can not through"]

let%spec "some specs" =
  1 + 1 [@eq 2]

(* open OUnit2  *)

(* let __fun_1 _ = *)
(*   let e = 1 in *)
(*   assert_bool e *)

(* let suite = "suite" >::: [ *)
(*   "test features" >:: __fun_1 *)
(* ] *)

(* let () = *)
(*   run_test_tt_main suite *)

