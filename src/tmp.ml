let spec = Spec.new_spec "hoge"
in
  (spec,
   (fun () ->
      (Spec.start spec;
       (let example = Spec.new_example "test"
        in
          (Spec.add_example example;
           Spec.start_example example;
           (try
              if "hoge" = "huga"
              then Spec.add_successful_expectation ()
              else Spec.add_failure_expectation "( = )" "hoge" "huga"
            with | e -> Spec.add_error (Printexc.to_string e));
           Spec.end_example example));
       Spec.remove_spec spec)))


