open OUnit2
open Math.Natural
open Math.Rational

let test_create_zero _ =
  let result = nat_to_int Z in
  assert_equal 0 result

let test_create_one _ =
  let result = nat_to_int (S Z) in
  assert_equal 1 result

let test_create_two _ =
  let result = nat_to_int (S (S Z)) in
  assert_equal 2 result

let test_natural_number_aliases _ =
  let result = nat_to_int one in
  assert_equal 1 result;

  let result = nat_to_int two in
  assert_equal 2 result;

  let result = nat_to_int three in
  assert_equal 3 result

let naturals_suite =
  "Natural numbers"
  >::: [
         "create zero" >:: test_create_zero;
         "create one" >:: test_create_one;
         "create two" >:: test_create_two;
         "natural number aliases" >:: test_natural_number_aliases;
       ]

let test_create_one_over_one _ =
  let result = frac_to_float (Fraction (S Z, S Z)) in
  assert_equal 1. result

let test_create_one_over_two _ =
  let result = frac_to_float (Fraction (S Z, S (S Z))) in
  assert_equal 0.5 result

let test_rational_number_aliases _ =
  let result = frac_to_float one_half in
  assert_equal 0.5 result;

  let result = frac_to_float one_quarter in
  assert_equal 0.25 result

let rationals_suite =
  "Rational numbers"
  >::: [
         "create one over one" >:: test_create_one_over_one;
         "create one over two" >:: test_create_one_over_two;
         "rational number aliases" >:: test_rational_number_aliases;
       ]

let () =
  run_test_tt_main naturals_suite;
  run_test_tt_main rationals_suite
