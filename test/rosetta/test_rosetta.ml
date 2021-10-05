open OUnit2

let test_asdf _ = assert_equal "asf" "sdf"

let rationals_suite =
  "Rational numbers" >::: [ "create one over one" >:: test_asdf ]

let () = run_test_tt_main rationals_suite
