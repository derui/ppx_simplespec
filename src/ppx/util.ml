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

(* Shortcut for [fun a b... -> e] as [fun_ ["a";"b"] e]*)
let fun_ args e =
  match args with
  | [] -> failwith "Invalid patterns to Util.fun_, only accept n >= 1"
  | pat :: rest ->
     let rec fun_' pats func =
       match pats with
       | [] -> func
       | pat :: rest -> fun_' rest (Exp.fun_ "" None (Pat.var {txt = pat;loc = !default_loc}) func) in
     fun_' rest (Exp.fun_ "" None (Pat.var {txt = pat;loc = !default_loc}) e)

let names_to_module_path paths =
  match paths with
  | [] -> failwith "need least one path to resolve module path"
  | f :: paths ->
    List.fold_left (fun ident path -> Ldot (ident, path)) (Lident f) paths

let to_ounit_fun ?(loc=Location.none) name = Exp.ident {txt = names_to_module_path ["OUnit2";name];loc}

module Exp = struct
  let is_ident = function
    | {pexp_desc = Pexp_ident _;_} -> true
    | _ -> false
  (* Check a expression is either identity or not *)
end
