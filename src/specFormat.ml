

(* expectationのそれぞれの値について、文字列への変換を行う *)
module type Formatter = sig
  val spec_format : Format.formatter -> Spec.t -> unit
end

module type S = sig

  val format : Format.formatter -> Spec.t -> unit

end

module Make(F:Formatter) : S = struct
  let format = F.spec_format
end

module Formatters = struct

  module Text = struct
    let spec_format fmt spec =

      let is_success = function
        | Spec.Successful -> true
        | _ -> false in

      let example_format fmt examples =
        let open Spec in
        let inner_example_format fmt example =
          let descr = example.Example.description
          and expectations = example.Example.expectations in
          let count = List.length expectations in
          let successes = List.filter is_success expectations in
          let failures = List.filter (fun e -> not (is_success e)) expectations in
          Format.fprintf fmt "@[%s [%d/%d]@\n" descr (List.length successes) count;
          List.iter (function
          | Successful -> ()
          | Failure (op, expect, active) -> Format.fprintf fmt "Failure : %s %s %s\n" expect op active
          | Error err -> Format.fprintf fmt "Error : %s\n" err
          ) failures;
          Format.fprintf fmt "@]"
        in
        List.iter (inner_example_format fmt) examples in

      let open Spec in
      let descr = spec.Spec.description
      and examples = spec.Spec.examples in
      Format.fprintf fmt "@[<2>%s@\n%a@]@." descr example_format examples;
  end

end

module Text = Make(Formatters.Text)
