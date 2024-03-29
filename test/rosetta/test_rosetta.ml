open OUnit2
open Rosetta.Ast
open Rosetta.Ast_processing
open Rosetta.Mapping
open Owl

let test_done _ =
  let result = compute_adj_list (Constructor (`Done, [])) in

  assert_equal result [ (State 0, [ (Done, 1.) ]) ]

let test_idle _ =
  let result = compute_adj_list (Constructor (`End, [])) in

  assert_equal result [ (State 0, [ (Idle, 1.) ]) ]

let test_send _ =
  let result =
    compute_adj_list
      (Constructor
         ( `Send,
           [ Constructor (`Apply [ "string" ], []); Constructor (`Done, []) ] ))
  in

  assert_equal result
    [ (State 1, [ (Done, 1.) ]); (State 0, [ (State 1, 1.) ]) ]

let test_receive _ =
  let result =
    compute_adj_list
      (Constructor
         ( `Receive,
           [ Constructor (`Apply [ "int" ], []); Constructor (`Done, []) ] ))
  in

  assert_equal result
    [ (State 1, [ (Done, 1.) ]); (State 0, [ (State 1, 1.) ]) ]

let test_branch _ =
  let result =
    compute_adj_list
      (Tagged
         ( `Branch,
           [
             ("Prob", Constructor (`Prob 0.6, []));
             ("True", Constructor (`Done, []));
             ("False", Constructor (`End, []));
           ] ))
  in

  assert_equal result
    [ (State 1, [ (Idle, 0.4); (Done, 0.6) ]); (State 0, [ (State 1, 1.) ]) ]

let test_branch_and_send _ =
  let result =
    compute_adj_list
      (Tagged
         ( `Branch,
           [
             ("Prob", Constructor (`Prob 0.6, []));
             ( "True",
               Constructor
                 ( `Send,
                   [
                     Constructor (`Apply [ "string" ], []);
                     Constructor (`Done, []);
                   ] ) );
             ( "False",
               Constructor
                 ( `Send,
                   [
                     Constructor (`Apply [ "string" ], []);
                     Constructor (`End, []);
                   ] ) );
           ] ))
  in

  assert_equal result
    [
      (State 3, [ (Idle, 1.) ]);
      (State 2, [ (Done, 1.) ]);
      (State 1, [ (State 3, 0.4); (State 2, 0.6) ]);
      (State 0, [ (State 1, 1.) ]);
    ]

let test_choice _ =
  let result =
    compute_adj_list
      (Tagged
         ( `Choice,
           [
             ("Prob", Constructor (`Prob 0.6, []));
             ("True", Constructor (`Done, []));
             ("False", Constructor (`End, []));
           ] ))
  in

  assert_equal result
    [ (State 1, [ (Idle, 0.4); (Done, 0.6) ]); (State 0, [ (State 1, 1.) ]) ]

let test_recursion _ =
  let result =
    compute_adj_list
      (Rec
         ( "X",
           Constructor
             (`Receive, [ Constructor (`Apply [ "string" ], []); RecVar "X" ])
         ))
  in

  assert_equal result
    [
      (State 2, [ (State 1, 1.) ]);
      (State 1, [ (State 2, 1.) ]);
      (State 0, [ (State 1, 1.) ]);
    ]

let test_buyer_seller_example _ =
  let result =
    compute_adj_list
      (Rec
         ( "X",
           Constructor
             ( `Receive,
               [
                 Constructor (`Apply [ "int" ], []);
                 Tagged
                   ( `Choice,
                     [
                       ("Prob", Constructor (`Prob 0.25, []));
                       ("True", Constructor (`Done, []));
                       ( "False",
                         Constructor
                           ( `Send,
                             [
                               Constructor (`Apply [ "int" ], []);
                               Tagged
                                 ( `Branch,
                                   [
                                     ("Prob", Constructor (`Prob 0.6, []));
                                     ("True", Constructor (`End, []));
                                     ("False", RecVar "X");
                                   ] );
                             ] ) );
                     ] );
               ] ) ))
  in

  assert_equal result
    [
      (State 5, [ (State 1, 0.4); (Idle, 0.6) ]);
      (State 4, [ (State 5, 1.) ]);
      (State 3, [ (State 4, 0.75); (Done, 0.25) ]);
      (State 2, [ (State 3, 1.) ]);
      (State 1, [ (State 2, 1.) ]);
      (State 0, [ (State 1, 1.) ]);
    ]

let success_prob_suite =
  "Success probability suite"
  >::: [
         "done" >:: test_done;
         "idle" >:: test_idle;
         "send" >:: test_send;
         "receive" >:: test_receive;
         "branch" >:: test_branch;
         "branch and send" >:: test_branch_and_send;
         "choice" >:: test_choice;
         "recursion" >:: test_recursion;
         "buyer seller example" >:: test_buyer_seller_example;
       ]

let test_done_mapping _ =
  let adj_list = [ (State 0, [ (Done, 1.) ]) ] in

  let result = map_adj_list adj_list in

  let exp_q = Mat.create 1 1 0. in
  let exp_r = Mat.create 1 2 0. in
  Mat.set exp_r 0 0 1.0;
  assert_equal (exp_q, exp_r) result

let test_idle_mapping _ =
  let adj_list = [ (State 0, [ (Idle, 1.) ]) ] in

  let result = map_adj_list adj_list in

  let exp_q = Mat.create 1 1 0. in
  let exp_r = Mat.create 1 2 0. in
  Mat.set exp_r 0 1 1.0;
  assert_equal (exp_q, exp_r) result

let test_send_mapping _ =
  let adj_list = [ (State 1, [ (Done, 1.) ]); (State 0, [ (State 1, 1.) ]) ] in

  let result = map_adj_list adj_list in

  let exp_q = Mat.create 2 2 0. in
  Mat.set exp_q 0 1 1.0;
  let exp_r = Mat.create 2 2 0. in
  Mat.set exp_r 1 0 1.0;
  assert_equal (exp_q, exp_r) result

let adj_list_mapping_suite =
  "Adjacency list mapping"
  >::: [
         "done" >:: test_done_mapping;
         "idle" >:: test_idle_mapping;
         "send" >:: test_send_mapping;
       ]

let test_complete_convex_sum_prob _ =
  let p = Constructor (`Prob 0.5, [])
  and q = Constructor (`Prob 1.0, [])
  and r = Constructor (`Prob 0.0, []) in

  let result = pp_convex_sum p q r in

  assert_equal "0.5" result

let test_partial_convex_sum_prob _ =
  let p = Var "p"
  and q = Constructor (`Prob 1.0, [])
  and r = Constructor (`Prob 0.0, []) in

  let result = pp_convex_sum p q r in

  assert_equal "conv_sum(p, 1., 0.)" result

let convex_sum_suite =
  "Convex sum parsing"
  >::: [
         "complete convex sum" >:: test_complete_convex_sum_prob;
         "partial convex sum" >:: test_partial_convex_sum_prob;
       ]

let () =
  run_test_tt_main success_prob_suite;
  run_test_tt_main adj_list_mapping_suite;
  run_test_tt_main convex_sum_suite
