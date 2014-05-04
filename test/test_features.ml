
describe "The number one" begin
  it "shoud to be implement late" pending;
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
;;

describe "The string" begin
  it "should equal test to 'test'" begin
    let a = "test" in
    a should = "test";
  end

end
;;

let r = ref 1 in
describe "Preparation and post-process each example" begin
  before each begin
    r := succ !r
  end;

  after each begin
    r := 1
  end;

  it "should run each spec that in on before and after" begin
    2 should = (!r)
  end;

  it "should run spec that in on before and after in twice" begin
    2 should = (!r)
  end;
end
;;

describe "Preparation and post-process" begin
  before all begin
    let ch = open_out "tmp.txt" in
    output_string ch  "hoge";
    close_out ch
  end;

  after all begin
    Sys.remove "tmp.txt"
  end;

  it "should run once before at first describe" begin
    (Sys.file_exists "tmp.txt") should = true
  end;
end
;;

describe "Matching with cases" begin
  it "should be able to match result" begin
    (Some 1) should match (Some 0 | Some 1);
  end;
end
;;

describe "Matching exception to raise" begin
  it "should be able to match exception" begin
    let f _ = failwith "func" in
    (f ()) should raise (Failure _);
  end;
end
