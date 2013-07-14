
let spec =
  describe "The number one" begin
    it "shoud to be implement late";
    it "should equal 2 when added to itself" begin
      (1 + 1) should = 2;
      let eq = (=) in
      (1 + 1) should eq 2;
      let eq x = (=) 2 x in
      (1 + 1) should eq;
    end;

    it "should equal 0 when subtract to itself" begin
      (1 - 1) should = 0;
      1 should (fun x y -> (1 - x) = y) 0;
      1 should (fun x -> (1 - x) = 0);
    end
  end

module F = struct
  let format_success = ""
  let format_failure op res expect =
    Printf.sprintf "  %s %s %s\n" op res expect
  let format_error e = Printf.sprintf "  error raised : %s\n" e
end

module Fmt = Simplespec.Format.Make(F)

let () =
  let s = (Simplespec.Spec.run_spec spec) in
  List.iter print_string (Simplespec.SpecFormat.format s)
;;
