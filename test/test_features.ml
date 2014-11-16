module Simplespec = Simplespec

let %spec "test features" =
  1 [@should (=) 1];
  "test" [@should (<>) "foobar"];
  let eq a b = a > b in
  1 [@should eq 2]

let () =
  let s = (Simplespec.Spec.run_specs ()) in
  Fmt.format Format.std_formatter s
