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

let suite =
  "Pick"
  >::: [
         "random client chooses true" >:: test_random_client_picks_true;
         "random client chooses false" >:: test_random_client_picks_false;
         "idle client chooses false" >:: test_idle_client_picks_false;
         "echo client chooses true" >:: test_echo_client_picks_true;
       ]

let () = run_test_tt_main suite
