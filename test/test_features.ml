
let%spec "boolean specs" =
  let e = 1 in
  (e = 1) [@true "equal"];
  (e = 2) [@false "not equal"]

let%spec "some specs" =
  1 + 1 [@eq 2];
  1 + 0 [@ne 2]

let%spec "raise exception" =
  let list = [1;2;3] in
  (fun () -> List.find ((=) 0) list) [@raises Not_found];
  let runner () = List.find ((=) 4) list in
  runner [@raises Not_found]
