open OUnit2
open ProFuse.Session.Bare

let echo_server ep =
  match branch ep with
  | `True ep ->
      let ep = send 42 ep in
      close ep
  | `False ep -> idle ep

let random_client ep =
  pick
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
  idle ep;
  0

let echo_client ep =
  let ep = select_true ep in
  let x, ep = receive ep in
  close ep;
  x

let test_random_client_picks_true _ =
  Random.init 1;
  (* seed forcing true *)
  let ep1, ep2 = create () in
  let _ = Thread.create echo_server ep1 in

  let res = random_client ep2 in

  assert_equal 42 res

let test_random_client_picks_false _ =
  Random.init 2;
  (* seed forcing false *)
  let ep1, ep2 = create () in
  let _ = Thread.create echo_server ep1 in

  let res = random_client ep2 in

  assert_equal 0 res

let test_idle_client_picks_false _ =
  let ep1, ep2 = create () in
  let _ = Thread.create echo_server ep1 in

  let res = idle_client ep2 in

  assert_equal 0 res

let test_echo_client_picks_true _ =
  let ep1, ep2 = create () in
  let _ = Thread.create echo_server ep1 in

  let res = echo_client ep2 in

  assert_equal 42 res

let pick_suite =
  "Pick"
  >::: [
         "random client chooses true" >:: test_random_client_picks_true;
         "random client chooses false" >:: test_random_client_picks_false;
         "idle client chooses false" >:: test_idle_client_picks_false;
         "echo client chooses true" >:: test_echo_client_picks_true;
       ]

let rec seller ep =
  let bid, ep = receive ep in
  pick
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
      pick
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

let examples_suite =
  "Examples"
  >::: [
         "buyer seller" >:: test_buyer_seller;
         "buyer seller no agreement" >:: test_buyer_seller_no_agreement;
       ]

let () =
  run_test_tt_main pick_suite;
  run_test_tt_main examples_suite
