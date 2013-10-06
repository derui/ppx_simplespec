let spec =
  let spec = Simplespec.Spec.Spec.new_spec "The number one"
  in
    (spec,
     (fun () ->
        (Simplespec.Spec.Spec.start_spec spec;
         Simplespec.Spec.Spec.run_all_preparations spec;
         (let example =
            Simplespec.Spec.Example.new_example "shoud to be implement late"
          in Simplespec.Spec.Spec.add_example example);
         (let example =
            Simplespec.Spec.Example.new_example
              "should equal 2 when added to itself"
          in
            (Simplespec.Spec.Spec.add_example example;
             Simplespec.Spec.Spec.run_each_preparations ();
             Simplespec.Spec.Spec.start_example example;
             (try
                if (1 + 1) = 2
                then Simplespec.Spec.Spec.add_successful_expectation ()
                else
                  Simplespec.Spec.Spec.add_failure_expectation "( = )"
                    "1 + 1" "2"
              with
              | e -> Simplespec.Spec.Spec.add_error (Printexc.to_string e));
             (let eq = ( = )
              in
                ((try
                    if eq (1 + 1) 2
                    then Simplespec.Spec.Spec.add_successful_expectation ()
                    else
                      Simplespec.Spec.Spec.add_failure_expectation "eq"
                        "1 + 1" "2"
                  with
                  | e ->
                      Simplespec.Spec.Spec.add_error (Printexc.to_string e));
                 let eq x = 2 = x
                 in
                   try
                     if eq (1 + 1)
                     then Simplespec.Spec.Spec.add_successful_expectation ()
                     else
                       Simplespec.Spec.Spec.add_failure_expectation "eq"
                         "1 + 1" "true"
                   with
                   | e ->
                       Simplespec.Spec.Spec.add_error (Printexc.to_string e)));
             Simplespec.Spec.Spec.end_example example;
             Simplespec.Spec.Spec.run_each_post_processes ()));
         (let example =
            Simplespec.Spec.Example.new_example
              "should equal 0 when subtract to itself"
          in
            (Simplespec.Spec.Spec.add_example example;
             Simplespec.Spec.Spec.run_each_preparations ();
             Simplespec.Spec.Spec.start_example example;
             (try
                if (1 - 1) = 0
                then Simplespec.Spec.Spec.add_successful_expectation ()
                else
                  Simplespec.Spec.Spec.add_failure_expectation "( = )"
                    "1 - 1" "0"
              with
              | e -> Simplespec.Spec.Spec.add_error (Printexc.to_string e));
             (try
                if (fun x y -> (1 - x) = y) 1 0
                then Simplespec.Spec.Spec.add_successful_expectation ()
                else
                  Simplespec.Spec.Spec.add_failure_expectation
                    "(fun x y -> (1 - x) = y)" "1" "1"
              with
              | e -> Simplespec.Spec.Spec.add_error (Printexc.to_string e));
             (try
                if (fun x -> (1 - x) = 0) 1
                then Simplespec.Spec.Spec.add_successful_expectation ()
                else
                  Simplespec.Spec.Spec.add_failure_expectation
                    "(fun x -> (1 - x) = 0)" "1" ""
              with
              | e -> Simplespec.Spec.Spec.add_error (Printexc.to_string e));
             Simplespec.Spec.Spec.end_example example;
             Simplespec.Spec.Spec.run_each_post_processes ()));
         Simplespec.Spec.Spec.run_all_post_processes spec;
         Simplespec.Spec.Spec.end_spec spec)))
  
let spec2 =
  let r = ref 1 in
  let spec = Simplespec.Spec.Spec.new_spec "Preparation and post-process"
  in
    (spec,
     (fun () ->
        (Simplespec.Spec.Spec.start_spec spec;
         Simplespec.Spec.Spec.run_all_preparations spec;
         Simplespec.Spec.Spec.add_preparation_for_each spec
           (fun () -> r := succ !r);
         Simplespec.Spec.Spec.add_post_process_for_each spec
           (fun () -> r := 1);
         (let example =
            Simplespec.Spec.Example.new_example
              "should run each spec that in on before and after"
          in
            (Simplespec.Spec.Spec.add_example example;
             Simplespec.Spec.Spec.run_each_preparations ();
             Simplespec.Spec.Spec.start_example example;
             (try
                if 2 = !r
                then Simplespec.Spec.Spec.add_successful_expectation ()
                else
                  Simplespec.Spec.Spec.add_failure_expectation "( = )" "2"
                    "!r"
              with
              | e -> Simplespec.Spec.Spec.add_error (Printexc.to_string e));
             Simplespec.Spec.Spec.end_example example;
             Simplespec.Spec.Spec.run_each_post_processes ()));
         Simplespec.Spec.Spec.run_all_post_processes spec;
         Simplespec.Spec.Spec.end_spec spec)))
  
let spec3 =
  let r = ref 1 in
  let spec = Simplespec.Spec.Spec.new_spec "Preparation and post-process"
  in
    (spec,
     (fun () ->
        (Simplespec.Spec.Spec.start_spec spec;
         Simplespec.Spec.Spec.run_all_preparations spec;
         Simplespec.Spec.Spec.add_preparation spec (fun () -> r := succ !r);
         (let example =
            Simplespec.Spec.Example.new_example
              "should run once before at first describe"
          in
            (Simplespec.Spec.Spec.add_example example;
             Simplespec.Spec.Spec.run_each_preparations ();
             Simplespec.Spec.Spec.start_example example;
             (try
                if 2 = !r
                then Simplespec.Spec.Spec.add_successful_expectation ()
                else
                  Simplespec.Spec.Spec.add_failure_expectation "( = )" "2"
                    "!r"
              with
              | e -> Simplespec.Spec.Spec.add_error (Printexc.to_string e));
             Simplespec.Spec.Spec.end_example example;
             Simplespec.Spec.Spec.run_each_post_processes ()));
         Simplespec.Spec.Spec.run_all_post_processes spec;
         Simplespec.Spec.Spec.end_spec spec)))
  
module Fmt = Simplespec.SpecFormat.Text
  
let () =
  let s = Simplespec.Spec.Spec.run_spec spec
  in
    (Fmt.format Format.std_formatter s;
     let s = Simplespec.Spec.Spec.run_spec spec2
     in
       (Fmt.format Format.std_formatter s;
        let s = Simplespec.Spec.Spec.run_spec spec3
        in Fmt.format Format.std_formatter s))
  

