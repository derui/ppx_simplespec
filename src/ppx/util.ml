open Ast_helper
open Ast_mapper
open Asttypes
open Parsetree
open Longident
open Location

let may_tuple tup = function
  | [] -> None
  | [x] -> Some x
  | l -> Some (tup ?loc:None ?attrs:None l)

let lid s = mkloc (Longident.parse s) !default_loc
let constr s args = Exp.construct (lid s) (may_tuple Exp.tuple args)
let nil () = constr "[]" []
let cons hd tl = constr "::" [hd; tl]
let list l = List.fold_right cons l (nil ())

let names_to_module_path paths =
  match paths with
  | [] -> failwith "need least one path to resolve module path"
  | f :: paths ->
    List.fold_left (fun ident path -> Ldot (ident, path)) (Lident f) paths

let to_ounit_fun ?(loc=Location.none) name = Exp.ident {txt = names_to_module_path ["OUnit2";name];loc}
