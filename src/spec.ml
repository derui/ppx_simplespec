type spec_result = Successful | Failure of string * string * string

module Example = struct

  type t = {
    description : string;
    mutable expectations : spec_result list;
    example_id : int;
  }

  let current_example = ref None
  let example_counter = ref 0
  let get_id () =
    let new_count = succ !example_counter in
    example_counter := new_count;
    new_count
  ;;

  let new_example desc = {
    description = desc;
    expectations = [];
    example_id = get_id ();
  }
  ;;

  let start_example e = current_example := Some e
  let end_example e = current_example := None

  let add_successful_expectation () =
    match !current_example with
    | None -> failwith "not set to current example"
    | Some e ->
      e.expectations <- Successful :: e.expectations;
      current_example := Some e
  ;;

  let add_failure_expectation ope_str result_str expect_str =
    match !current_example with
    | None -> failwith "not set to current example"
    | Some e ->
      e.expectations <- Failure (ope_str, result_str, expect_str) :: e.expectations;
      current_example := Some e
  ;;
end

module Spec = struct
  type t = {
    description : string;
    mutable examples : Example.t list;
    spec_id : int;
  }

  let spec_stack = Stack.create ()
  let spec_counter = ref 0
  let get_spec_id () =
    let new_count = succ !spec_counter in
    spec_counter := new_count;
    new_count
  ;;

  let new_spec (desc: string) =
    {description = desc; examples = [];
     spec_id = get_spec_id ()
    }

  (* let run_spec (specs: t list) = *)
  (*   {specs with result = List.fold_left (fun res spec -> *)
  (*     let running = List.map (fun e -> e ()) spec.expects in *)
  (*     if List.for_all (fun x -> id (fst x)) running then *)
  (*       Successful :: res *)
  (*     else *)
  (*       Failure (List.map snd (List.filter (fun x -> not (fst x)) running)) :: res *)
  (*    ) [] *)
  (*   } *)

  let start spec = Stack.push spec_stack spec
  let remove_spec spec = Stack.pop spec_stack

  let add_example e =
    let spec = Stack.top spec_stack in
    spec.examples <- e :: spec.examples;
  ;;

  let start_example e =
    let spec = Stack.top spec_stack in
    let e = List.find (fun ex -> ex.Example.example_id == e.Example.example_id)
      spec.examples in

    Example.start_example e;
  ;;

  let remove_example e = Example.end_example e
  ;;

  let add_successful_expectation = Example.add_successful_expectation
  let add_failure_expectation = Example.add_failure_expectation

end

(* Specモジュールの中身はそのまま展開される *)
include Spec
