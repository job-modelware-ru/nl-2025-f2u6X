open OUnit2

let test_add _ = assert_equal 3 (1+2)

let () = run_test_tt_main (
  "suite" >:: [
    "add" >:: test_add;
  ])