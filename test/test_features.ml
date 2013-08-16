
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

module Fmt = Simplespec.SpecFormat.Text

let () =
  let s = (Simplespec.Spec.Spec.run_spec spec) in
  Fmt.format Format.std_formatter s
;;
