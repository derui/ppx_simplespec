open Ast_helper
open Ast_mapper
open Asttypes
open Parsetree
open Longident
open Location

module U = Util

type labels = Cmp of expression

let labels_to_tup = function
  | Cmp e -> ("cmp", e)

(* assert_equal and others *)
let attrs_to_labels attrs =
  let attr_to_label = function
    | ({txt = "cmp";loc}, PStr [{pstr_desc = Pstr_eval ({pexp_desc = e;_},_);_}]) ->
       let e = Exp.mk ~loc e in Some (Cmp (e))
    | ({txt;_}, _) -> Printf.printf "Unknown attribute %s\n" txt; None
  in

  let rec convert attrs labels =
    match attrs with
    | [] -> labels
    | attr :: rest -> convert rest (attr_to_label attr :: labels) in

  let labels = convert attrs [] in
  List.filter (function
  | None -> false
  | Some _ -> true
  ) labels |> List.map (function
  | Some e -> e
  | None -> failwith "Not contain None in this pont?"
  )

let assert_equal loc exp = function
  | PStr ([
    {pstr_desc = Pstr_eval ({pexp_desc = e ;_}, _); pstr_loc = loc}
  ]) -> begin
    Exp.apply ~loc (U.to_ounit_fun ~loc "assert_equal") [("", Exp.mk ~loc e); ("", exp)]
  end
  | _ -> failwith "@eq only accept expression"

(* assert_not_equal *)
let invert_comparator loc e =
  U.fun_ ["a";"b"]
    (Exp.apply ~loc (Exp.ident (U.lid "not")) [
      ("", Exp.apply e [("", Exp.ident (U.lid "a"));("", Exp.ident (U.lid "b"))])
    ])

let assert_not_equal loc exp = function
  | PStr ([
    {pstr_desc = Pstr_eval ({pexp_desc = e ;_}, attrs); pstr_loc = loc}
  ]) -> begin
    let options = attrs_to_labels attrs in
    let have_cmp = List.exists (function
      | Cmp _ -> true
    ) options in
    (* If not to be specified any comparetor, use (=) to inverse result *)
    let options = if not have_cmp then
        Cmp (Exp.ident (U.lid "=")) :: options
      else
        options in
    let options = List.map (function
      | Cmp e -> Cmp (invert_comparator loc e)
    ) options in
    let options = List.map labels_to_tup options in
    let args = List.rev (("", exp) :: ("", Exp.mk ~loc e) :: options) in
    Exp.apply ~loc (U.to_ounit_fun ~loc "assert_equal") args
  end
  | _ -> failwith "@ne only accept expression"
