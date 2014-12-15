open Ast_helper
open Ast_mapper
open Asttypes
open Parsetree
open Longident
open Location

module U = Util
module EQ = Assert_equal_conv

(* expression to assertion. *)
let payload_to_txt attr_name = function
  | PStr [{pstr_desc = Pstr_eval (e, _);_}] -> begin
    match e with
    | {pexp_desc = Pexp_constant (Const_string (str, _));_} ->
       str
    | _ -> failwith (Printf.sprintf "%s is only accept string" attr_name)
  end
  | _ -> attr_name

(* assert_bool and derived *)
let assert_true loc exp description =
  let description = Exp.constant (Const_string (description, None)) in
  Exp.apply ~loc (U.to_ounit_fun ~loc "assert_bool") [("", description) ;("", exp)]
let assert_false loc exp description =
  let description = Exp.constant (Const_string (description, None)) in
  Exp.apply ~loc (U.to_ounit_fun ~loc "assert_bool")
    [("", description);
     ("", Exp.apply ~loc (Exp.ident {txt = Lident "not";loc}) [("", exp)])]

(* assert_raises and derived *)
let assert_raises loc exp = function
  | PStr [{pstr_desc = Pstr_eval (e, _);_}] -> begin
    Printf.printf "hoge";
    match e with
    | {pexp_desc = Pexp_construct (ident, e');_} ->
       Exp.apply ~loc (U.to_ounit_fun ~loc "assert_raises")
         [("", e);("", exp)]
    | _ -> failwith "[@raises] can only accept constructor for Exception"
  end
  | _ -> failwith "[@raises] should apply with expression"

module AssertType = struct
  type t = Asty_true of attribute
           | Asty_false of attribute
           | Asty_equal of attribute
           | Asty_not_equal of attribute
           | Asty_raises of attribute

  let of_attribute = function
    | ({txt = "true";loc}, _) as p -> Some(Asty_true p)
    | ({txt = "false";loc}, _) as p -> Some(Asty_false p)
    | ({txt = "eq";loc}, _) as p -> Some(Asty_equal p)
    | ({txt = "ne";loc}, _) as p -> Some(Asty_not_equal p)
    | ({txt = "raises";loc}, _) as p -> Some(Asty_raises p)
    | _ -> None
end

let attr_to_assertion exp = function
  | ({txt = "true";loc}, payload) -> payload_to_txt "assert_true" payload |>  assert_true loc exp
  | ({txt = "false";loc}, payload) -> payload_to_txt "assert_false" payload |> assert_false loc exp
  | ({txt = "eq";loc}, payload) -> EQ.assert_equal loc exp payload
  | ({txt = "ne";loc}, payload) -> EQ.assert_not_equal loc exp payload
  | ({txt = "raises";loc}, payload) -> assert_raises loc exp payload
  | _ -> exp                    (* return only expression without attributes *)

let rec attrs_to_assertion exp attrs assertions =
  match attrs with
  | [] -> assertions
  | [attr] -> attr_to_assertion exp attr :: assertions 
  | attr :: rest -> attrs_to_assertion exp rest (attr_to_assertion exp attr :: assertions)

let convert_attributes ~loc exp attrs =
  let assertions = attrs_to_assertion exp attrs [] in
  match List.rev assertions with
  | [] -> exp
  | [assertion] -> assertion
  | assertion :: rest -> List.fold_left (fun memo asrt -> Exp.sequence memo asrt) assertion rest

let rec contains_assert = function
  | [] -> false
  | [attr] -> begin match AssertType.of_attribute attr with
    | Some _ -> true
    | None -> false
  end
  | attr :: rest -> begin match AssertType.of_attribute attr with
    | Some _ -> true
    | None -> contains_assert rest
  end
     
