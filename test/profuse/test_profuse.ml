open OUnit2
open ProFuse.Session.Bare
open Math.Rational

let const_42_server ep =
  match branch ep with
  | `True ep ->
      let ep = send 42 ep in
      close ep
  | `False ep -> idle ep

let coin_flip_recv_client ep =
  pick one_half
    (fun ep ->
      let ep = select_false ep in
      idle ep;
      0)
    (fun ep ->
      let ep = select_true ep in
      let x, ep = receive ep in
      close ep;
      x)
    ep

let idle_client ep =
  let ep = select_false ep in
  idle ep

let recv_client ep =
  let ep = select_true ep in
  let x, ep = receive ep in
  close ep;
  x

let test_random_client_picks_true _ =
  Random.init 1;
  (* seed forcing true *)
  let ep1, ep2 = create () in
  let _ = Thread.create const_42_server ep1 in

  let res = coin_flip_recv_client ep2 in

  assert_equal 42 res

let test_random_client_picks_false _ =
  Random.init 4;
  (* seed forcing false *)
  let ep1, ep2 = create () in
  let _ = Thread.create const_42_server ep1 in

  let res = coin_flip_recv_client ep2 in

  assert_equal 0 res

let test_idle_client_picks_false _ =
  let ep1, ep2 = create () in
  let _ = Thread.create const_42_server ep1 in

  let res = idle_client ep2 in

  assert_equal () res

let test_recv_client_picks_true _ =
  let ep1, ep2 = create () in
  let _ = Thread.create const_42_server ep1 in

  let res = recv_client ep2 in

  assert_equal 42 res

let double_false_success_client ep =
  pick one_half
    (fun ep ->
      let ep = select_false ep in
      let ep = select_false ep in
      close ep;
      true)
    (fun ep ->
      let ep = select_true ep in
      let ep = select_true ep in
      idle ep;
      false)
    ep

let double_false_success_server ep =
  match branch ep with
  | `True ep -> (
      match branch ep with `True ep -> idle ep | `False ep -> idle ep)
  | `False ep -> (
      match branch ep with `True ep -> idle ep | `False ep -> close ep)

let test_double_false_picks_false _ =
  Random.init 4;
  (* seed forcing initial choice to be false *)
  let ep1, ep2 = create () in
  let _ = Thread.create double_false_success_server ep1 in

  let result = double_false_success_client ep2 in

  assert_equal true result

let test_double_false_picks_true _ =
  Random.init 0;
  (* seed forcing initial choice to be true *)
  let ep1, ep2 = create () in
  let _ = Thread.create double_false_success_server ep1 in

  let result = double_false_success_client ep2 in

  assert_equal false result

let pick_suite =
  "Pick"
  >::: [
         "random client chooses true" >:: test_random_client_picks_true;
         "random client chooses false" >:: test_random_client_picks_false;
         "idle client chooses false" >:: test_idle_client_picks_false;
         "receiving client chooses true" >:: test_recv_client_picks_true;
         "double false chooses false" >:: test_double_false_picks_false;
         "double false chooses true" >:: test_double_false_picks_true;
       ]

let rec seller ep =
  let bid, ep = receive ep in
  pick one_half
    (fun ep ->
      let ep = select_false ep in
      close ep)
    (fun ep ->
      let ep = select_true ep in
      let ep = send (bid + 10) ep in
      match branch ep with `True ep -> seller ep | `False ep -> idle ep)
    ep

let rec buyer ep offer =
  let ep = send offer ep in
  match branch ep with
  | `True ep ->
      let counteroffer, ep = receive ep in
      pick one_half
        (fun ep ->
          let ep = select_false ep in
          idle ep;
          -1)
        (fun ep ->
          let ep = select_true ep in
          buyer ep counteroffer)
        ep
  | `False ep ->
      close ep;
      offer

let test_buyer_seller _ =
  Random.init 1;
  (* seed forcing agreement *)
  let ep1, ep2 = create () in
  let _ = Thread.create seller ep1 in

  let offer = buyer ep2 42 in

  assert_equal ~printer:string_of_int 62 offer

let test_buyer_seller_no_agreement _ =
  Random.init 3;
  (* seed forcing no agreement *)
  let ep1, ep2 = create () in
  let _ = Thread.create seller ep1 in

  let offer = buyer ep2 42 in

  assert_equal ~printer:string_of_int (-1) offer

let echo_server ep =
  match branch ep with
  | `True ep ->
      let x, ep = receive ep in
      let ep = send x ep in
      close ep
  | `False ep -> idle ep

let echo_client ep x =
  let ep = select_true ep in
  let ep = send x ep in
  let x, ep = receive ep in
  close ep;
  x

let coin_flip_echo_client ep x =
  pick one_half
    (fun ep ->
      idle_client ep;
      None)
    (fun ep -> Some (echo_client ep x))
    ep

let test_echo_client _ =
  let ep1, ep2 = create () in
  let _ = Thread.create echo_server ep1 in

  let reply = echo_client ep2 42 in

  assert_equal 42 reply

let test_idle_client _ =
  let ep1, ep2 = create () in
  let _ = Thread.create echo_server ep1 in

  let reply = idle_client ep2 in

  assert_equal () reply

let test_coin_flip_echo_client_picks_true _ =
  Random.init 3;
  (* seed forcing true *)
  let ep1, ep2 = create () in
  let _ = Thread.create echo_server ep1 in

  let reply = coin_flip_echo_client ep2 42 in

  assert_equal (Some 42) reply

let test_coin_flip_echo_client_picks_false _ =
  Random.init 4;
  (* seed forcing false *)
  let ep1, ep2 = create () in
  let _ = Thread.create echo_server ep1 in

  let reply = coin_flip_echo_client ep2 42 in

  assert_equal None reply

let examples_suite =
  "Examples"
  >::: [
         "echo client" >:: test_echo_client;
         "idle client" >:: test_idle_client;
         "coin flip echo client picks true"
         >:: test_coin_flip_echo_client_picks_true;
         "coin flip echo client picks false"
         >:: test_coin_flip_echo_client_picks_false;
         "buyer seller" >:: test_buyer_seller;
         "buyer seller no agreement" >:: test_buyer_seller_no_agreement;
       ]

let inversion epX epY =
  match branch_2ch epX epY with
  | `True (epX, epY) -> (
      let epY = select_false epY in
      close epY;
      match branch epX with `True epX -> close epX | `False epX -> idle epX)
  | `False (epX, epY) -> (
      let epY = select_true epY in
      idle epY;
      match branch epX with `True epX -> close epX | `False epX -> idle epX)

let inversion_client epX epY =
  pick one_half
    (fun epX ->
      let epX = select_false epX in
      match branch_2ch epY epX with
      | `True (epY, epX) ->
          idle epY;
          let epX = select_true epX in
          close epX;
          true
      | `False (epY, epX) ->
          close epY;
          let epX = select_false epX in
          idle epX;
          false)
    (fun epX ->
      let epX = select_true epX in
      match branch_2ch epY epX with
      | `True (epY, epX) ->
          idle epY;
          let epX = select_true epX in
          close epX;
          true
      | `False (epY, epX) ->
          close epY;
          let epX = select_false epX in
          idle epX;
          false)
    epX

let test_inversion_client_picks_false _ =
  Random.init 4;
  (* seed forcing initial choice to be false *)
  let epX1, epX2 = create () in
  let epY1, epY2 = create () in
  let _ = Thread.create (fun (epX, epY) -> inversion epX epY) (epX1, epY1) in

  let choice = inversion_client epX2 epY2 in

  assert_equal true choice

let test_inversion_client_picks_true _ =
  Random.init 0;
  (* seed forcing initial choice to be true *)
  let epX1, epX2 = create () in
  let epY1, epY2 = create () in
  let _ = Thread.create (fun (epX, epY) -> inversion epX epY) (epX1, epY1) in

  let choice = inversion_client epX2 epY2 in

  assert_equal false choice

let multi_channel_suite =
  "Multi-channel"
  >::: [
         "inversion client picks false" >:: test_inversion_client_picks_false;
         "inversion client picks true" >:: test_inversion_client_picks_true;
       ]

let receiver ep =
  let _, ep = receive ep in
  close ep

let sender ep =
  let ep = send 42 ep in
  close ep

let send_and_receive ?(st = cst_placeholder) () =
  let ep1, ep2 = create ~st () in
  let _ = Thread.create receiver ep1 in
  sender ep2

let coin_flip_server ?(st = cst_placeholder) () ep =
  match branch ep with
  | `True ep ->
      close ep;
      send_and_receive ~st ()
  | `False ep ->
      idle ep;
      send_and_receive ~st ()

let coin_flipper ep =
  pick one_half
    (fun ep ->
      let ep = select_false ep in
      idle ep;
      false)
    (fun ep ->
      let ep = select_true ep in
      close ep;
      true)
    ep

let test_same_session_type_combination _ =
  Random.init 1;
  (* seed forcing initial choice to be true *)
  let ep1, ep2 = create () in
  let _ = Thread.create (coin_flip_server ()) ep1 in

  let result = coin_flipper ep2 in

  assert_equal false result

let const_42_server_reply ?(st = cst_placeholder) () =
  let ep1, ep2 = create ~st () in
  let _ = Thread.create const_42_server ep1 in
  recv_client ep2

let coin_flip_server_mix_sessions ?(st = cst_placeholder) () ep =
  match branch_2st ep st with
  | `True (ep, stX) ->
      close ep;
      send_and_receive ~st:stX ()
  | `False (ep, stY) ->
      idle ep;
      let _ = const_42_server_reply ~st:stY () in
      ()

let test_diff_session_type_branch_combination _ =
  Random.init 0;
  (* seed forcing initial choice to be true *)
  let ep1, ep2 = create () in
  let _ = Thread.create (coin_flip_server_mix_sessions ()) ep1 in

  let result = coin_flipper ep2 in

  assert_equal false result

let coin_flipper_mix_sessions ?(st = cst_placeholder) () ep =
  pick_2st one_half
    (fun ep stX ->
      let ep = select_false ep in
      idle ep;
      let _ = send_and_receive ~st:stX () in
      false)
    (fun ep stY ->
      let ep = select_true ep in
      close ep;
      let _ = const_42_server_reply ~st:stY () in
      true)
    ep st

let test_diff_session_type_pick_combination _ =
  Random.init 0;
  (* seed forcing initial choice to be true *)
  let ep1, ep2 = create () in
  let _ = Thread.create (coin_flip_server ()) ep1 in

  let result = coin_flipper ep2 in

  assert_equal false result

let closed_session_combination =
  "Closed session combination"
  >::: [
         "same session type" >:: test_same_session_type_combination;
         "different session type in branch"
         >:: test_diff_session_type_branch_combination;
         "different session type in pick"
         >:: test_diff_session_type_branch_combination;
       ]

let () =
  run_test_tt_main pick_suite;
  run_test_tt_main examples_suite;
  run_test_tt_main multi_channel_suite

(* Code examples *)

let two_sessions_pick_single_branch epX epY =
  pick one_half
    (fun epX ->
      let epX = select_false epX in
      idle epX;
      let epY = send false epY in
      close epY)
    (fun epX ->
      let epX = select_true epX in
      close epX)
    epX

let two_sessions_pick_both_branches epX epY =
  pick one_half
    (fun epX ->
      let epX = select_false epX in
      idle epX;
      let epY = send false epY in
      close epY)
    (fun epX ->
      let epX = select_true epX in
      close epX;
      let epY = send true epY in
      close epY)
    epX

(* let invalid_two_ep_pick_example epX epY =
   pick one_half
     (fun epX ->
       let epX = select_false epX in
       idle epX;
       let epY = send false epY in
       close epY)
     (fun epX ->
       let epX = select_true epX in
       close epX;
       let epY = send 42 epY in
       close epY)
     epX *)

(* let invalid_two_ep_pick_example_2 epX epY =
   pick one_half
     (fun epX ->
       let epX = select_false epX in
       idle epX;
       let epY = select_false epY in
       let epY = send false epY in
       close epY)
     (fun epX ->
       let epX = select_true epX in
       close epX;
       let epY = select_true epY in
       let epY = send 42 epY in
       close epY)
     epX *)

let two_ep_pick_example_2 epX epY =
  pick_2ch one_half
    (fun epX epY ->
      let epX = select_false epX in
      idle epX;
      let epY = select_false epY in
      send false epY)
    (fun epX epY ->
      let epX = select_true epX in
      close epX;
      let epY = select_true epY in
      send 42 epY)
    epX epY

let run_echo_client_example ?(st = cst_placeholder) () =
  let ep1, ep2 = create ~st () in
  let _ = Thread.create echo_server ep1 in
  echo_client ep2 42


let run_coin_flip_echo_client_example ?(st = cst_placeholder) () =
  let ep1, ep2 = create ~st () in
  let _ = Thread.create echo_server ep1 in
  coin_flip_echo_client ep2 42