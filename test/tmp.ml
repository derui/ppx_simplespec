let spec =
  let spec = Spec.new_spec "The number one"
  in
    (spec,
     (fun () ->
        (Spec.start_spec spec;
         (let example = Spec.new_example "shoud to be implement late"
          in Spec.add_example example);
         (let example =
            Spec.new_example "should equal 2 when added to itself"
          in
            (Spec.add_example example;
             Spec.start_example example;
             (try
                if (1 + 1) = 2
                then Spec.add_successful_expectation ()
                else Spec.add_failure_expectation "( = )" "1 + 1" "2"
              with | e -> Spec.add_error (Printexc.to_string e));
             (let eq = ( = )
              in
                ((try
                    if eq (1 + 1) 2
                    then Spec.add_successful_expectation ()
                    else Spec.add_failure_expectation "eq" "1 + 1" "2"
                  with | e -> Spec.add_error (Printexc.to_string e));
                 let eq x = 2 = x
                 in
                   try
                     if eq (1 + 1)
                     then Spec.add_successful_expectation ()
                     else Spec.add_failure_expectation "eq" "1 + 1" "true"
                   with | e -> Spec.add_error (Printexc.to_string e)));
             Spec.end_example example));
         (let example =
            Spec.new_example "should equal 0 when subtract to itself"
          in
            (Spec.add_example example;
             Spec.start_example example;
             (try
                if (1 - 1) = 0
                then Spec.add_successful_expectation ()
                else Spec.add_failure_expectation "( = )" "1 - 1" "0"
              with | e -> Spec.add_error (Printexc.to_string e));
             (try
                if (fun x y -> (1 - x) = y) 1 0
                then Spec.add_successful_expectation ()
                else
                  Spec.add_failure_expectation "(fun x y -> (1 - x) = y)" "1"
                    "1"
              with | e -> Spec.add_error (Printexc.to_string e));
             (try
                if (fun x -> (1 - x) = 0) 1
                then Spec.add_successful_expectation ()
                else
                  Spec.add_failure_expectation "(fun x -> (1 - x) = 0)" "1"
                    ""
              with | e -> Spec.add_error (Printexc.to_string e));
             Spec.end_example example));
         Spec.end_spec spec)))
  
let () = ignore (Pa_simplespec.Spec.run_spec spec)
  

