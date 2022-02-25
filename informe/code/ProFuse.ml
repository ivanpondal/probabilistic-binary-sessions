module ProbEcho = struct
  (*BEGIN*SimpleProbEchoService*)
  let echo_service ep =
    match Session.branch ep with
    | `True ep ->
        let x, ep = Session.receive ep in
        let ep = Session.send x ep in
        Session.close ep
    | `False ep -> Session.idle ep
  (*END*SimpleProbEchoService*)

  (*BEGIN*SimpleProbEchoClient*)
  let echo_client ep x =
    let ep = Session.select_true ep in
    let ep = Session.send x ep in
    let x, ep = Session.receive ep in
    Session.close ep;
    x
  (*END*SimpleProbEchoClient*)

  (*BEGIN*SimpleIdleClient*)
  let idle_client ep =
    let ep = Session.select_false ep in
    Session.idle ep
  (*END*SimpleIdleClient*)

  (*BEGIN*SimpleCoinFlipEchoClient*)
  let coin_flip_echo_client ep x =
    Session.pick Rational.one_half
      (fun ep ->
        idle_client ep;
        None)
      (fun ep -> Some (echo_client ep x))
      ep
  (*END*SimpleCoinFlipEchoClient*)

  (*BEGIN*SimpleCoinFlipEchoMain*)
  let _ =
    let a, b = Session.create () in
    let _ = Thread.create echo_server a in
    coin_flip_echo_client b 42
  (*END*SimpleCoinFlipEchoMain*)

  (*BEGIN*TwoSessionsPickSingleBranch*)
  let two_sessions_pick_single_branch epX epY =
    Session.pick Rational.one_half
      (fun epX ->
        let epX = Session.select_false epX in
        Session.idle epX;
        let epY = Session.send false epY in
        Session.close epY)
      (fun epX ->
        let epX = Session.select_true epX in
        Session.close epX)
      epX
  (*END*TwoSessionsPickSingleBranch*)

  (*BEGIN*TwoSessionsPickBothBranches*)
  let two_sessions_pick_both_branches epX epY =
    Session.pick Rational.one_half
      (fun epX ->
        let epX = Session.select_false epX in
        Session.idle epX;
        let epY = Session.send false epY in
        Session.close epY)
      (fun epX ->
        let epX = Session.select_true epX in
        Session.close epX;
        let epY = Session.send true epY in
        Session.close epY)
      epX
  (*END*TwoSessionsPickBothBranches*)

  (*BEGIN*TwoSessionsInvalidPickBothBranches*)
  let two_sessions_invalid_pick epX epY =
    Session.pick Rational.one_half
      (fun epX ->
        let epX = Session.select_false epX in
        Session.idle epX;
        let epY = Session.send false epY in
        Session.close epY)
      (fun epX ->
        let epX = Session.select_true epX in
        Session.close epX;
        let epY = Session.send 42 epY in (* error de tipado *)
        Session.close epY)
      epX
  (*END*TwoSessionsInvalidPickBothBranches*)

  (*BEGIN*TwoSessionsInvalidPickBothBranchesWithSelect*)
  let two_sessions_invalid_pick epX epY =
    Session.pick Rational.one_half
      (fun epX ->
        let epX = Session.select_false epX in
        Session.idle epX;
        let epY = Session.select_false epY in
        let epY = Session.send false epY in
        Session.close epY)
      (fun epX ->
        let epX = Session.select_true epX in
        Session.close epX;
        let epY = Session.select_true epY in (* error de tipado *)
        let epY = Session.send 42 epY in
        Session.close epY)
      epX
  (*END*TwoSessionsInvalidPickBothBranchesWithSelect*)

  (*BEGIN*TwoSessionsPickTwo*)
  let two_sessions_valid_pick2 epX epY =
    Session.pick_2ch Rational.one_half
      (fun epX epY ->
        let epX = Session.select_false epX in
        Session.idle epX;
        let epY = Session.select_false epY in
        let epY = Session.send false epY in
        Session.close epY)
      (fun epX epY ->
        let epX = Session.select_true epX in
        Session.close epX;
        let epY = Session.select_true epY in
        let epY = Session.send 42 epY in
        Session.close epY)
      epX epY
  (*END*TwoSessionsPickTwo*)

  (*BEGIN*EchoClientClosedSession*)
  let run_echo_client_example ?(st = cst_placeholder) () =
    let ep1, ep2 = Session.create ~st () in
    let _ = Thread.create echo_service ep1 in
    echo_client ep2 42
  (*END*EchoClientClosedSession*)

  (*BEGIN*CoinFlipEchoClientClosedSession*)
  let run_coin_flip_echo_client_example
                              ?(st = cst_placeholder) () =
    let ep1, ep2 = Session.create ~st () in
    let _ = Thread.create echo_server ep1 in
    coin_flip_echo_client ep2 42
  (*END*CoinFlipEchoClientClosedSession*)

  (*BEGIN*PickIdleCloseRunEchoClient*)
  let pick_idle_close_and_run_echo_client
                              ?(st = cst_placeholder) () ep =
    Session.pick Rational.one_half
      (fun ep ->
        let ep = Session.select_false ep in
        Session.idle ep;
        run_echo_client_example ~st ())
      (fun ep ->
        let ep = Session.select_true ep in
        Session.close ep;
        run_echo_client_example ~st ())
      ep
  (*END*PickIdleCloseRunEchoClient*)

  (*BEGIN*InvalidPickIdleCloseRunEchoClientOrCoinFlip*)
  let pick_idle_close_and_run_echo_client_or_coin_flip
                              ?(st = cst_placeholder) () ep =
    Session.pick Rational.one_half
      (fun ep ->
        let ep = Session.select_false ep in
        Session.idle ep;
        run_echo_client_example ~st ())
      (fun ep ->
        let ep = Session.select_true ep in
        Session.close ep;
        match run_coin_flip_echo_client_example ~st () with
        (* error de tipado *)
        | Some result -> result
        | None -> 0)
      ep
  (*END*InvalidPickIdleCloseRunEchoClientOrCoinFlip*)

  (*BEGIN*ValidPickIdleCloseRunEchoClientOrCoinFlip*)
  let pick_idle_close_and_run_echo_client_or_coin_flip
                              ?(st = cst_placeholder) () ep =
    Session.pick_2st Rational.one_half
      (fun ep st ->
        let ep = Session.select_false ep in
        Session.idle ep;
        run_echo_client_example ~st ())
      (fun ep st ->
        let ep = Session.select_true ep in
        Session.close ep;
        match run_coin_flip_echo_client_example ~st () with
        | Some result -> result
        | None -> 0)
      ep st
  (*END*ValidPickIdleCloseRunEchoClientOrCoinFlip*)

  (*BEGIN*NoOptionalValidPickIdleCloseRunEchoClientOrCoinFlip*)
  let pick_idle_close_and_mix_run_echo_client ep =
    Session.pick Rational.one_half
      (fun ep ->
        let ep = Session.select_false ep in
        Session.idle ep;
        run_echo_client_example ())
      (fun ep ->
        let ep = Session.select_true ep in
        Session.close ep;
        match run_coin_flip_echo_client_example () with
        | Some result -> result
        | None -> 0)
      ep
  (*END*NoOptionalValidPickIdleCloseRunEchoClientOrCoinFlip*)
end
