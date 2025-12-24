open Alcotest

let test_add () =
  check int "same int" 3 (1 + 2)

let () =
  run "myapp-test" [ 
    "math", [
      test_case "add" `Quick test_add
    ];
  ]