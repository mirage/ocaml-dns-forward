
let first () =
  Lwt_main.run begin
    Alcotest.(check (list string)) "foo" ["a"; "b"] ["a"; "b"];
    Lwt.return ()
  end

let a_set = [
  "First",       `Quick, first;
]

let () =
  Alcotest.run "dns-forward" [
    "A", a_set;
  ]
