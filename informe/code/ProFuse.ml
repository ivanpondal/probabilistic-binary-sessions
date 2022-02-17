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
    let a, b = create () in
    let _ = Thread.create echo_server a in
    coin_flip_echo_client b 42
  (*END*SimpleCoinFlipEchoMain*)

  (*BEGIN*TwoSessionsPickSingleBranch*)
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
  (*END*TwoSessionsPickSingleBranch*)

  (*BEGIN*TwoSessionsPickBothBranches*)
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
  (*END*TwoSessionsPickBothBranches*)

  (*BEGIN*TwoSessionsInvalidPickBothBranches*)
  let two_sessions_invalid_pick epX epY =
    pick one_half
      (fun epX ->
        let epX = select_false epX in
        idle epX;
        let epY = send false epY in
        close epY)
      (fun epX ->
        let epX = select_true epX in
        close epX;
        let epY = send 42 epY in (* error de tipado *)
        close epY)
      epX
  (*END*TwoSessionsInvalidPickBothBranches*)

  (*BEGIN*TwoSessionsInvalidPickBothBranchesWithSelect*)
  let two_sessions_invalid_pick epX epY =
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
        let epY = select_true epY in (* error de tipado *)
        let epY = send 42 epY in
        close epY)
      epX
  (*END*TwoSessionsInvalidPickBothBranchesWithSelect*)

  (*BEGIN*TwoSessionsPickTwo*)
  let two_sessions_valid_pick2 epX epY =
    pick_2ch one_half
      (fun epX epY ->
        let epX = select_false epX in
        idle epX;
        let epY = select_false epY in
        let epY = send false epY in
        close epY)
      (fun epX epY ->
        let epX = select_true epX in
        close epX;
        let epY = select_true epY in
        let epY = send 42 epY in
        close epY)
      epX epY
  (*END*TwoSessionsPickTwo*)

  (*BEGIN*EchoClientClosedSession*)
  let run_echo_client_example ?(st = cst_placeholder) () =
    let ep1, ep2 = create ~st () in
    let _ = Thread.create echo_service ep1 in
    echo_client ep2 42
  (*END*EchoClientClosedSession*)

  (*BEGIN*CoinFlipEchoClientClosedSession*)
  let run_coin_flip_echo_client_example
                              ?(st = cst_placeholder) () =
    let ep1, ep2 = create ~st () in
    let _ = Thread.create echo_server ep1 in
    coin_flip_echo_client ep2 42
  (*END*CoinFlipEchoClientClosedSession*)
end
