

(* expectationのそれぞれの値について、文字列への変換を行う *)
module type Formatter = sig
  val spec_format : Format.formatter -> Spec.Spec.t list -> unit
end

module type S = sig

  val format : Format.formatter -> Spec.Spec.t list -> unit

end

module Make(F:Formatter) : S = struct
  let format = F.spec_format
end

module Formatters = struct

  module Text = struct
    let spec_format fmt specs =

      let is_success = function
        | Spec.Successful -> true
        | _ -> false in

      let result_format fmt results =
        let open Spec in
        let count = ref 0
        and successes = ref 0
        and failures = ref 0 in
        let inner_result_format fmt (name, result) =
          count := succ !count;
          successes := if is_success result then succ !successes else !successes;
          failures := if not (is_success result) then succ !failures else !failures;
          match result with
          | Successful -> ()
          | Failure -> Format.fprintf fmt "%s@\n" name
          | Error err -> Format.fprintf fmt "Error@ :@ %s@\n" err
        in
        List.iter (inner_result_format fmt) results in

      let open Spec in
      List.iter (fun {Spec.name = name; recorder = recorder} ->
        let name = if name = "" then "no spec name" else name in
        Format.fprintf fmt "@[<2>%s@\n%a@]@." name result_format recorder.Recorder.specs
      ) specs

  end
end

module Text = Make(Formatters.Text)
