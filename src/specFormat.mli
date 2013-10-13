
module type Formatter = sig
  val spec_format : Format.formatter -> Spec.t -> unit
end

module type S = sig

  val format : Format.formatter -> Spec.t -> unit

end

module Make(F:Formatter) : S

module Text : S
