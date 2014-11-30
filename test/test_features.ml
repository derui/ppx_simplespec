
let%spec "test features" =

  let e  = 1 in
  e [@true][@false];
  e [@eq 2]

(* open OUnit2  *)

(* let __fun_1 _ = *)
(*   let e = 1 in *)
(*   assert_bool e *)

(* let suite = "suite" >::: [ *)
(*   "test features" >:: __fun_1 *)
(* ] *)

(* let () = *)
(*   run_test_tt_main suite *)

