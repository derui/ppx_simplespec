
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

module Make(F:Formatter) : S
