type spec_result = Successful
                   | Failure of string * string * string
                   | Error of string

(* Spec内における、一つのitに対応するモジュール
   内部には、内部には、それぞれのit内における各shouldの結果を格納している。
*)
module Example = struct

  type t = {
    description : string;
    body : t -> unit;
    mutable expectations : spec_result list;
    example_id : int;
  }

  let get_id =
    let example_counter = ref 0 in
    fun () -> 
      let new_count = succ !example_counter in
      example_counter := new_count;
      new_count

  let new_example desc body = {
    description = desc;
    expectations = [];
    body;
    example_id = get_id ();
  }

  let run_example example = example.body example

  let add_successful_expectation example =
    example.expectations <- Successful :: example.expectations

  let add_failure_expectation example ope_str result_str expect_str =
    example.expectations <- Failure (ope_str, result_str, expect_str) :: example.expectations

  let add_error example str =
    example.expectations <- Error str :: example.expectations

end

(* 各describeに対応する型を提供する
   内部には、それぞれのitに対応するExampleのリストを保持している。
*)
module Spec = struct
  type process = unit -> unit
  type example = Example.t

  type t = {
    description : string;
    mutable examples : example list;
    spec_id : int;
    make_examples : t -> unit;
    all_preparations : process Queue.t;
    each_preparations : process Queue.t;
    all_post_processes : process Queue.t;
    each_post_processes : process Queue.t;
  }

  let spec_stack = Stack.create ()
  let get_spec_id =
    let spec_counter = ref 0 in
    fun () ->
      let new_count = succ !spec_counter in
      spec_counter := new_count;
      new_count

  let new_spec (desc: string) body =
    {description = desc; examples = [];
     spec_id = get_spec_id ();
     all_preparations = Queue.create ();
     all_post_processes = Queue.create ();
     each_preparations = Queue.create ();
     each_post_processes = Queue.create ();
     make_examples = body;
    }

  let with_spec spec f =
    Stack.push spec spec_stack ;
    f spec;
    Stack.pop spec_stack

  let add_example e =
    let spec = Stack.top spec_stack in
    spec.examples <- e :: spec.examples

  let new_example = Example.new_example
  let add_successful_expectation = Example.add_successful_expectation
  let add_failure_expectation = Example.add_failure_expectation
  let add_error = Example.add_error

  let add_preparation spec f = Queue.add f spec.all_preparations
  let add_preparation_for_each spec f = Queue.add f spec.each_preparations
  let add_post_process spec f = Queue.add f spec.all_post_processes
  let add_post_process_for_each spec f = Queue.add f spec.each_post_processes

  let run_each_preparations spec = 
    Queue.iter (fun f -> f()) spec.each_preparations

  let run_each_post_processes spec =
    Queue.iter (fun f -> f ()) spec.each_post_processes

  let run_all_preparations spec = 
    Queue.iter (fun f -> f()) spec.all_preparations

  let run_all_post_processes spec =
    Queue.iter (fun f -> f ()) spec.all_post_processes

  let run_spec spec =
    spec.make_examples spec;
    run_all_preparations spec;
    List.iter (fun e ->
      run_each_preparations spec;
      Example.run_example e;
      run_each_post_processes spec
    ) spec.examples;
    run_all_post_processes spec;
    spec

end

include Spec
