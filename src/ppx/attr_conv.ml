open Ast_helper
open Ast_mapper
open Asttypes
open Parsetree
open Longident
open Location

module U = Util

(* expression to assertion. *)
let assert_true loc exp = Exp.apply ~loc (U.to_ounit_fun ~loc "assert_true") [("", exp)]
let assert_false loc exp = Exp.apply ~loc (U.to_ounit_fun ~loc "assert_true")
       [("", Exp.apply ~loc (Exp.ident {txt = Lident "not";loc}) [("", exp)])]

let assert_equal loc exp = function
  | PStr ([{pstr_desc =
      Pstr_eval ({pexp_desc = e ;_}, _);
            pstr_loc = loc
           }]) -> begin
    Exp.apply ~loc (U.to_ounit_fun ~loc "assert_equal") [("", Exp.mk ~loc e); ("", exp)]
  end
  | _ -> failwith "@eq only accept expression"

let attr_to_assertion exp = function
  | ({txt = "true";loc},_) -> assert_true loc exp
  | ({txt = "false";loc},_) -> assert_false loc exp
  | ({txt = "eq";loc}, payload) -> assert_equal loc exp payload
  | _ -> failwith ""

let rec attrs_to_assertion exp attrs assertions =
  match attrs with
  | [] -> assertions
  | [attr] -> attr_to_assertion exp attr :: assertions 
  | attr :: rest -> attrs_to_assertion exp rest (attr_to_assertion exp attr :: assertions)

let convert_attributes ~loc strc exp attrs =
  let assertions = attrs_to_assertion exp attrs [] in
  match List.rev assertions with
  | [] -> strc
  | [assertion] -> assertion
  | assertion :: rest -> List.fold_left (fun memo asrt -> Exp.sequence memo asrt) assertion rest
