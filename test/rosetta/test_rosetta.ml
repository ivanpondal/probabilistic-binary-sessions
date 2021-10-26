open OUnit2
open Rosetta.Ast

let test_done _ =
  let result = compute_adj_list (Constructor (`Done, [])) in

  assert_equal result [ (Init, [ (Done, 1.) ]) ]

let test_idle _ =
  let result = compute_adj_list (Constructor (`End, [])) in

  assert_equal result [ (Init, [ (Idle, 1.) ]) ]

let test_send _ =
  let result =
    compute_adj_list
      (Constructor
         ( `Send,
           [ Constructor (`Apply [ "string" ], []); Constructor (`Done, []) ] ))
  in

  assert_equal result [ (State 0, [ (Done, 1.) ]); (Init, [ (State 0, 1.) ]) ]

let test_receive _ =
  let result =
    compute_adj_list
      (Constructor
         ( `Receive,
           [ Constructor (`Apply [ "int" ], []); Constructor (`Done, []) ] ))
  in

  assert_equal result [ (State 0, [ (Done, 1.) ]); (Init, [ (State 0, 1.) ]) ]

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
    [ (State 0, [ (Idle, 0.4); (Done, 0.6) ]); (Init, [ (State 0, 1.) ]) ]

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
      (State 2, [ (Idle, 1.) ]);
      (State 1, [ (Done, 1.) ]);
      (State 0, [ (State 2, 0.4); (State 1, 0.6) ]);
      (Init, [ (State 0, 1.) ]);
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
    [ (State 0, [ (Idle, 0.4); (Done, 0.6) ]); (Init, [ (State 0, 1.) ]) ]

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
      (State 1, [ (State 0, 1.) ]);
      (State 0, [ (State 1, 1.) ]);
      (Init, [ (State 0, 1.) ]);
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
                       ( "True",
                         Constructor
                           ( `Send,
                             [
                               Constructor (`Apply [ "int" ], []);
                               Tagged
                                 ( `Branch,
                                   [
                                     ("Prob", Constructor (`Prob 0.6, []));
                                     ("True", RecVar "X");
                                     ("False", Constructor (`End, []));
                                   ] );
                             ] ) );
                       ("False", Constructor (`Done, []));
                     ] );
               ] ) ))
  in

  assert_equal result
    [
      (State 4, [ (Idle, 0.4); (State 0, 0.6) ]);
      (State 3, [ (State 4, 1.) ]);
      (State 2, [ (Done, 0.75); (State 3, 0.25) ]);
      (State 1, [ (State 2, 1.) ]);
      (State 0, [ (State 1, 1.) ]);
      (Init, [ (State 0, 1.) ]);
    ]

let success_prob_suite =
  "Success probability suite"
  >::: [
         "Done" >:: test_done;
         "Idle" >:: test_idle;
         "Send" >:: test_send;
         "Receive" >:: test_receive;
         "Branch" >:: test_branch;
         "Branch and send" >:: test_branch_and_send;
         "Choice" >:: test_choice;
         "Recursion" >:: test_recursion;
         "Buyer seller example" >:: test_buyer_seller_example;
       ]

let () = run_test_tt_main success_prob_suite
