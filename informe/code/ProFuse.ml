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

  (*BEGIN*ValidTwoChannelPick*)
  let two_channel_pick_example epX epY =
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
  (*END*ValidTwoChannelPick*)
end
