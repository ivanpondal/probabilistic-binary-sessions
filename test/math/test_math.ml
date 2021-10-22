open OUnit2
open Math.Natural
open Math.Rational
open Math.Markov
open Owl

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

let test_paper_example_absortion_matrix _ =
  (* create Q matrix *)
  let q = Mat.create 4 4 0. in
  Mat.set q 0 1 1.;
  Mat.set q 1 2 (3. /. 4.);
  Mat.set q 2 3 1.;
  Mat.set q 3 0 (1. /. 3.);
  (* create R matrix *)
  let r = Mat.create 4 2 0. in
  Mat.set r 1 0 0.25;
  Mat.set r 3 1 (2. /. 3.);

  let b = absortion_matrix q r in

  (* create expected B matrix *)
  let expected_b = Mat.create 4 2 0. in
  Mat.set expected_b 0 0 (1. /. 3.);
  Mat.set expected_b 0 1 (2. /. 3.);
  Mat.set expected_b 1 0 (1. /. 3.);
  Mat.set expected_b 1 1 (2. /. 3.);
  Mat.set expected_b 2 0 (1. /. 9.);
  Mat.set expected_b 2 1 (8. /. 9.);
  Mat.set expected_b 3 0 (1. /. 9.);
  Mat.set expected_b 3 1 (8. /. 9.);
  assert_equal expected_b b

let markov_suite =
  "Markov"
  >::: [
         "paper example absortion matrix"
         >:: test_paper_example_absortion_matrix;
       ]

let () =
  run_test_tt_main naturals_suite;
  run_test_tt_main rationals_suite;
  run_test_tt_main markov_suite
