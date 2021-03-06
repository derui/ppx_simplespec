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
  let fn = Exp.fun_ Nolabel None (Pat.any ()) e in
  Str.value ~loc Nonrecursive [Vb.mk ~loc (Pat.var ~loc {txt = label;loc}) fn]

(* Make test suite from specs registered with extension *)
let make_test_suite () =
  let make_test_with_spec (label, test_name) = 
    Exp.apply (Util.to_ounit_fun ">::")
      [(Nolabel, Exp.constant (Const.string label));
       (Nolabel, Exp.ident {txt = Lident test_name;loc = Location.none})] in
  let test = List.map make_test_with_spec !specs |> Util.list in
  let test = Exp.apply (Util.to_ounit_fun "test_list") [(Nolabel, test)] in
  Str.value Nonrecursive [Vb.mk (Pat.var {txt = name_of_suite;loc = Location.none}) test]

(* Mapper to reconstruct for assertions that are change to some assertion method *)
let rec assertion_mapper = {default_mapper with
  expr = fun mapper strc ->
    let loc = strc.pexp_loc
    and attrs = strc.pexp_attributes in
    let e = strc.pexp_desc in
    if A.contains_assert attrs then
      A.convert_attributes ~loc (Exp.mk ~loc e) attrs
    else
      default_mapper.expr assertion_mapper strc
}

let spec_mapper argv =
  { default_mapper with
    structure = (fun mapper strc ->
      match strc with
      | {pstr_desc = Pstr_extension (({txt = "suite";loc}, payload), _);_} :: rest -> begin
        match payload with
        | PStr (strc) -> begin
          let structure = List.map (mapper.structure_item mapper) strc in
          let ident = Util.to_ounit_fun "run_test_tt_main" in
          let suite = make_test_suite () in
          let runner = Str.eval (
            Exp.apply ident [(Nolabel, Exp.ident {txt = Lident name_of_suite; loc = Location.none})]
          ) in
          (runner :: suite :: (List.rev structure)) |> List.rev |> fun l -> List.append l (default_mapper.structure mapper rest)
        end
        | _ -> default_mapper.structure mapper rest
      end
      | _ -> default_mapper.structure mapper strc
    );
    structure_item = (fun mapper strc ->
      match strc with
      | {pstr_desc =
          Pstr_extension (({ txt = "spec"; loc}, pstr),_);_} ->
         begin match pstr with
         | PStr [{pstr_desc = Pstr_value (_, [{pvb_pat = pat;pvb_expr = e;_}])}] ->
            begin match pat with
            | {ppat_desc = Ppat_constant (Pconst_string (str,_));_} ->
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
