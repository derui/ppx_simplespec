open Camlp4.PreCast
open Syntax
open Camlp4.Struct

(* These functions are taken from OSpec source (include comment as follows) *)
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
      Simplespec.Spec.add_successful_expectation example
    else
      Simplespec.Spec.add_failure_expectation example $str:str_op$ $str:str_res$ $str:str_exp$;
  with e -> Simplespec.Spec.add_error example (Printexc.to_string e)
  >>
;;

(* shouldにIdentityが渡された場合のexpectationを行う *)
let identifier_expectation _loc op res exp =
  let str_op = string_of_ident op in
  let str_res = string_of_expr res in
  let str_exp = string_of_expr exp in
  <:expr<
  try
    if $id:op$ $res$ $exp$ then
      Simplespec.Spec.add_successful_expectation example
    else
      Simplespec.Spec.add_failure_expectation example $str:str_op$ $str:str_res$ $str:str_exp$;
  with e -> Simplespec.Spec.add_error example (Printexc.to_string e)
  >>
;;

(* shouldにIdentityが渡された場合のexpectationを行う *)
let identifier_expectation_oneof _loc op res =
  let str_op = string_of_ident op in
  let str_res = string_of_expr res in
  <:expr<
  try
    if $id:op$ $res$ then
      Simplespec.Spec.add_successful_expectation example
    else
      Simplespec.Spec.add_failure_expectation example $str:str_op$ $str:str_res$ "true";
  with e -> Simplespec.Spec.add_error example (Printexc.to_string e)
  >>
;;

let rec mkfun _loc args body =
  match args with
  | p :: ps -> <:expr< fun $p$ -> $mkfun _loc ps body$ >>
  | [] -> <:expr< $body$ >>
;;

(* shouldにIdentityが渡された場合のexpectationを行う *)
let function_expectation _loc args body res exp =
  let f = mkfun _loc args body in
  let str_fun = "(" ^ string_of_expr f ^ ")" in
  let str_res = string_of_expr res in
  let v, str_exp =
    match exp with
    | Some e ->
      (<:expr< $f$ $res$ $e$ >>, <:expr< $str:str_res$ >>)
    | None ->
      (<:expr< $f$ $res$ >>, <:expr< "" >>)
  in
  <:expr<
  try
    if $v$ then
      Simplespec.Spec.add_successful_expectation example
    else
      Simplespec.Spec.add_failure_expectation example $str:str_fun$ $str:str_res$ $str_exp$;
  with e -> Simplespec.Spec.add_error example (Printexc.to_string e)
  >>
;;

(* shouldにIdentityが渡された場合のexpectationを行う *)
let function_expectation_oneof _loc op res =
  let str_op = string_of_expr op in
  let str_res = string_of_expr res in
  <:expr<
  try
    if $op$ $res$ then
      Simplespec.Spec.add_successful_expectation example
    else
      Simplespec.Spec.add_failure_expectation example $str:str_op$ $str:str_res$ "true";
  with e -> Simplespec.Spec.add_error example (Printexc.to_string e)
  >>
;;

(* shouldに比較演算子が渡された場合のexpectationを行う *)
let infixop_expectation_with_string _loc op res exp =
  let str_op = string_of_expr op in
  <:expr<
  try
    if $op$ $str:res$ $str:exp$ then
      Simplespec.Spec.add_successful_expectation example
    else
      Simplespec.Spec.add_failure_expectation example $str:str_op$ $str:res$ $str:exp$;
  with e -> Simplespec.Spec.add_error example (Printexc.to_string e)
  >>
;;


(* itブロックの中身をexampleとして実行する  *)
let to_example_block _loc desc seq =
  <:expr<
  let example = Simplespec.Spec.new_example $str:desc$ (fun example ->
    $Ast.exSem_of_list seq$
  ) in
  Simplespec.Spec.add_example spec example
  >>
;;

(* 空のitブロックの中身を登録する *)
let to_pending_example_block _loc desc =
  <:expr<
  let example = Simplespec.Spec.new_example $str:desc$ (fun _ -> ()) in
  Simplespec.Spec.add_example spec example
  >>
;;

(* describeブロック一つをspecとして作成する  *)
let to_spec _loc desc (seq : Ast.expr list) =
  <:expr<
  Simplespec.Spec.new_spec $str:desc$ (fun spec ->
    $Ast.exSem_of_list seq$;
  )
  >>
;;

let before_all_block _loc (seq : Ast.expr list) =
  <:expr<
  Simplespec.Spec.add_preparation spec (fun () ->
    $Ast.exSem_of_list seq$
  )
>>
;;

let after_all_block _loc (seq : Ast.expr list) =
  <:expr<
  Simplespec.Spec.add_post_process spec (fun () ->
    $Ast.exSem_of_list seq$
  )
>>
;;

let before_each_block _loc (seq : Ast.expr list) =
  <:expr<
  Simplespec.Spec.add_preparation_for_each spec (fun () ->
    $Ast.exSem_of_list seq$
  )
>>
;;

let after_each_block _loc (seq : Ast.expr list) =
  <:expr<
  Simplespec.Spec.add_post_process_for_each spec (fun () ->
    $Ast.exSem_of_list seq$
  )
>>
;;

EXTEND Gram
  expr: LEVEL "simple" [
    [ "describe"; des = STRING ; "begin" ; seq = LIST0 expr; "end" -> to_spec _loc des seq
    | "it" ; des = STRING ; "pending" -> to_pending_example_block _loc des
    | "it" ; des = STRING ; "begin" ; seq = LIST0 expr; "end" -> to_example_block _loc des seq
    (* 比較演算子と文字列リテラル *)
    | res = STRING ; "should" ; OPT "be" ; op = infixop0; exp = STRING ->
      infixop_expectation_with_string _loc op res exp
    (* =や<>などの比較演算子 *)
    | res = SELF ; "should" ; OPT "be" ; op = infixop0; exp = SELF ->
      infixop_expectation _loc op res exp
    (* 定義された2引数の関数 *)
    | res = SELF ; "should" ; OPT "be" ; f = ident; exp = SELF ->
      identifier_expectation _loc f res exp
    (* 定義された1引数の関数 *)
    | res = SELF ; "should" ; OPT "be" ; f = ident ->
      identifier_expectation_oneof _loc f res
    (* 関数定義 *)
    | res = SELF ; "should" ; OPT "be" ; "(" ; "fun" ; args = LIST1 ipatt; "->" ;
      e = expr ; ")" ; exp = OPT expr LEVEL "top" -> function_expectation _loc args e res exp;
    | "before"; "all" ; "begin" ; seq = LIST0 expr; "end" -> before_all_block _loc seq;
    | "before"; "each"; "begin"; seq = LIST0 expr; "end" -> before_each_block _loc seq;
    | "after"; "all"; "begin"; seq = LIST0 expr; "end" -> after_all_block _loc seq;
    | "after"; "each"; "begin"; seq = LIST0 expr; "end" -> after_each_block _loc seq;
    ]
  ];
END
