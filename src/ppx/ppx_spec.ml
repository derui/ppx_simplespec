open Ast_helper
open Ast_mapper
open Asttypes
open Parsetree
open Longident
open Location

module A = Attr_conv

exception Error of Location.t

let () =
  Location.register_error_of_exn (fun exn ->
    match exn with
    | Error loc ->
      Some (error ~loc "[%spec] accepts a structure, e.g. [%spec \"USER\"] or let%spec \"USER\" = ...")
    | _ -> None)

let specs = ref []

let name_of_suite = "suite"

(* Helpers to make test function on automation *)
let unique_id = ref 0
let unique_fun_id () =
  let ret = Printf.sprintf "__simplespec_%d" !unique_id in
  unique_id := succ !unique_id;
  ret

(* Make test function on top level *)
let make_test_fun loc label e =
  let fn = Exp.fun_"" None (Pat.any ()) e in
  Str.value ~loc Nonrecursive [Vb.mk ~loc (Pat.var ~loc {txt = label;loc}) fn]

(* Make test suite from specs registered with extension *)
let make_test_suite () =
  let make_test_with_spec (label, test_name) = 
    Exp.apply (Util.to_ounit_fun ">::")
      [("", Exp.constant (Const_string (label, None)));
       ("", Exp.ident {txt = Lident test_name;loc = Location.none})] in
  let test = List.map make_test_with_spec !specs |> Util.list in
  let test = Exp.apply (Util.to_ounit_fun "test_list") [("", test)] in
  Str.value Nonrecursive [Vb.mk (Pat.var {txt = name_of_suite;loc = Location.none}) test]

(* Mapper to reconstruct for assertions that are change to some assertion method *)
let rec assertion_mapper = {default_mapper with
  expr = fun mapper strc ->
    let loc = strc.pexp_loc
    and attrs = strc.pexp_attributes in
    match strc with
    | { pexp_desc = Pexp_ident lid;_} -> 
       A.convert_attributes ~loc strc (Exp.ident ~loc lid) attrs
    | {pexp_desc = Pexp_apply (e, args);_} ->
       A.convert_attributes ~loc strc (Exp.apply ~loc e args) attrs
    | _ -> default_mapper.expr assertion_mapper strc
}

let spec_mapper argv =
  { default_mapper with
    structure = (fun mapper strc ->
      match strc with
      | {pstr_desc = Pstr_attribute (({txt;loc}, _));_} :: _ -> [List.hd strc |> mapper.structure_item mapper]
      | _ -> begin
        let structure = List.map (mapper.structure_item mapper) strc in
        let ident = Util.to_ounit_fun "run_test_tt_main" in
        let suite = make_test_suite () in
        let runner = Str.eval (
          Exp.apply ident [("", Exp.ident {txt = Lident name_of_suite; loc = Location.none})]
        ) in
        let structure = List.rev structure in
        let structure = (runner :: suite :: structure) |> List.rev in
        structure
      end
    );
    structure_item = (fun mapper strc ->
      match strc with
      | {pstr_desc =
          Pstr_extension (({ txt = "spec"; loc}, pstr),_);_} ->
        begin match pstr with
        | PStr [{pstr_desc = Pstr_value (_, [{pvb_pat = pat;pvb_expr = e;_}])}] ->
          begin match pat with
          | {ppat_desc = Ppat_constant (Const_string (str,_));_} ->
            let fun_id = unique_fun_id () in
            specs := (str, fun_id) :: !specs;
            make_test_fun loc fun_id (assertion_mapper.expr mapper e)
          | _ -> failwith "spec must contain constant let "
          end
        | _ -> failwith "spec have to be extension for structure"
        end
      | _ -> default_mapper.structure_item mapper strc)
  }

let () = run_main spec_mapper
