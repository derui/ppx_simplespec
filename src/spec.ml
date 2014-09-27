type result = Successful
            | Failure
            | Error of string
type spec_result = string * result

module Recorder = struct
  type t = {
    mutable specs : spec_result list
  }

  let empty () = {specs = []}
end

module Spec = struct

  type t = {
    name : string;
    recorder : Recorder.t;
  }

  let get_spec name = {name;recorder = Recorder.empty ()}

  let record_result ?name spec expectation =
    let name = match name with None -> "" | Some n -> n in
    let recorder = spec.recorder in
    try
      if expectation then
        recorder.Recorder.specs <- (name, Successful) :: recorder.Recorder.specs
      else
        recorder.Recorder.specs <- (name, Failure) :: recorder.Recorder.specs
    with e ->
      recorder.Recorder.specs <- (name, Error (Printexc.to_string e)) :: recorder.Recorder.specs

end

type spec_name = string
type spec = Spec.t * (Spec.t -> unit)

let _all_specs = ref ([] : spec list)

let add_spec name spec =
  let recorder = Spec.get_spec name in
  _all_specs := (recorder, spec) :: !_all_specs

let run_specs () = 
  List.map (fun (spec, expectation) ->
            expectation spec;
            spec) !_all_specs
