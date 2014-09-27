open Ast_helper
open Ast_mapper
open Asttypes
open Parsetree
open Longident
open Location

exception Error of Location.t

let () =
  Location.register_error_of_exn (fun exn ->
    match exn with
    | Error loc ->
      Some (error ~loc "[%describe] accepts a structure, e.g. [%describe \"USER\"]")
    | _ -> None)

let rec uncurry_fun ~loc ?attrs patterns exp =
  match attrs with
  | None -> begin
    match patterns with
    | [] -> exp
    | pattern :: patterns -> Exp.fun_ ~loc "" None pattern (uncurry_fun ~loc patterns exp)
  end
  | Some attrs ->  begin
    match patterns with
    | [] -> exp
    | pattern :: patterns -> Exp.fun_ ~loc ~attrs "" None pattern (uncurry_fun ~loc ~attrs patterns exp)
  end

let apply_fun ~loc fname patterns = Exp.apply ~loc (Exp.ident {txt = Lident fname;loc}) patterns

let rec should_mapper = {default_mapper with
  expr = fun mapper strc ->
    match strc with
    | { pexp_desc = Pexp_constant constant;
        pexp_loc = loc;
        pexp_attributes = [({txt = "should";_}, strc)]} ->
      begin match strc with
      | PStr [{pstr_desc = Pstr_eval ({ pexp_loc  = loc;
                                        pexp_desc = Pexp_apply (ap, args);_;
                                      }, _)}] ->
        let args = List.rev (("", Exp.constant constant) :: (List.rev args)) in
        apply_fun ~loc "Simplespec.Spec.record_result" [
          ("", Exp.ident {txt = Lident "spec";loc});
          ("", Exp.apply ~loc ap args)]
      | _ -> failwith "should must be locate after expression"
      end
    | _ -> default_mapper.expr should_mapper strc
}

let describe_mapper argv =
  { default_mapper with
    structure_item = (fun mapper strc ->
      match strc with
      | {pstr_desc =
          Pstr_extension (({ txt = "spec"; loc}, pstr),_);_} ->
        begin match pstr with
        | PStr [{pstr_desc = Pstr_value (_, [{pvb_pat = pat;pvb_expr = e;_}])}] ->
          begin match pat with
          | {ppat_desc = Ppat_constant (Const_string (str,_));_} ->
            Str.eval ~loc
              (Exp.apply ~loc (Exp.ident {txt = Lident "Simplespec.Spec.add_spec";loc})
                 [("", Exp.constant (Const_string (str, None)));
                  ("", uncurry_fun ~loc [Pat.var {txt = "spec"; loc}] (should_mapper.expr mapper e))])
          | _ -> failwith "spec must contain constant let "
          end
        | _ -> failwith "spec have to be extension for structure"
        end
      | _ -> default_mapper.structure_item mapper strc)
    ;
        (* PStr [{ pstr_desc = *)
        (*         Pstr_eval ({ pexp_loc  = loc; *)
        (*                      pexp_desc = *)
        (*                      Pexp_constant (Const_string (sym, None))}, _)}] -> *)
        (* (\* Replace with a constant string with the value from the environment. *\) *)
        (* Exp.constant ~loc (Const_string (getenv sym, None)) *)
        (*   | _ -> raise (Error loc) *)
        (*   end *)
        (* (\* Delegate to the default mapper. *\) *)
        }

        let () = run_main describe_mapper
