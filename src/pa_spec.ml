open Camlp4.PreCast
open Syntax

(* These functions are taken from OSpec source (include comment follow) *)
(*
 * The string_of_* functions are taken from
 * http://caml.inria.fr/pub/ml-archives/caml-list/2008/08/a6c9c42fbb20ce51984d26cc54b61c30.en.html
 *)

let printer =
  let module P = Camlp4.Printers.OCaml.Make(Syntax) in
  new P.printer ()

let format_to_string (f : Format.formatter -> 'a -> unit) (v : 'a) : string =
  let buf = Buffer.create 128 in
  let fmt = Format.formatter_of_buffer buf in
  f fmt v;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

let string_of_expr : Ast.expr -> string = format_to_string printer#expr
let string_of_ident : Ast.ident -> string = format_to_string printer#ident
let string_of_patt : Ast.patt -> string = format_to_string printer#patt

(* shouldに比較演算子が渡された場合のexpectationを行う *)
let infixop_expectation _loc op res exp =
  let str_op = string_of_expr op in
  let str_res = string_of_expr res in
  let str_exp = string_of_expr exp in
  <:expr<
  try
    if $op$ $res$ $exp$ then
      Spec.add_successful_expectation ()
    else
      Spec.add_failure_expectation _loc $str:str_op$ $str:str_res$ $str:str_exp$;
  with e -> Spec.add_error (Printexc.to_string e)
  >>
;;

(* itブロックの中身をexampleとして実行する  *)
let to_example_block _loc desc seq =
  <:expr<
  let example = Spec.new_example $str:desc$ in
  begin
    Spec.add_example example;
 
    Spec.start_example example;
    $Ast.exSem_of_list seq$;
    Spec.end_example example;
  end
 >>
;;

(* 空のitブロックの中身を登録する *)
let to_pending_example_block _loc desc =
  <:expr<
  let example = Spec.new_example $str:desc$ in
  Spec.add_example example
  >>
;;

(* describeブロック一つをspecとして作成する  *)
let to_spec _loc desc (seq : Ast.expr list) =
  <:expr<
  let spec = Spec.new_spec $str:desc$ in
  (spec, (fun () ->
    Spec.start spec;
    $Ast.exSem_of_list seq$;
    Spec.remove_spec spec
   ))
 >>
;;
  
EXTEND Gram
  expr: LEVEL "simple" [
    [ "describe"; des = STRING ; "do" ; seq = LIST0 expr; "done" -> to_spec _loc des seq
    | "it" ; des = STRING ; "do" ; seq = LIST0 expr; "done" -> to_example_block _loc des seq
    | "it" ; des = STRING -> to_pending_example_block _loc des
    | res = SELF ; "should" ; OPT "be" ; op = infixop0; exp = SELF ->
      infixop_expectation _loc op res exp
    ]
  ];
END

