
type success_formatter = string
type failure_formatter = string -> string -> string -> string
type error_formatter = string -> string

(* expectationのそれぞれの値について、文字列への変換を行う *)
module type Formatter = sig
  val format_success : success_formatter
  val format_failure : failure_formatter
  val format_error : error_formatter
end

module type S = sig

  (* formatting Spec.t to string list per elements are as line. *)
  val format : Spec.Spec.t -> string list

end

module Make(F:Formatter) : S = struct
  let is_success = function
    | Spec.Successful -> true
    | _ -> false
  ;;

  let example_format example =
    let open Spec in
    let descr = example.Example.description
    and expectations = example.Example.expectations in
    let count = List.length expectations in
    let successes = List.filter is_success expectations in
    let default_output = [ Printf.sprintf "  %s [%d/%d]\n" descr (List.length successes) count] in
    let failures = List.filter (fun e -> not (is_success e)) expectations in
    List.rev (List.fold_right (fun e el ->
      let str = match e with
        | Successful -> F.format_success
        | Failure (op, expect, active) -> F.format_failure op expect active
        | Error err -> F.format_error err in
      str :: el
    ) failures default_output)
  ;;

  let format spec =
    let open Spec in
    let descr = spec.Spec.description 
    and examples = spec.Spec.examples in
    let descr_output = Printf.sprintf "%s\n" descr in
    descr_output :: (List.flatten (List.map example_format examples))
end 
