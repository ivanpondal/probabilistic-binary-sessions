module ProbEcho = struct
  (*BEGIN*SimpleProbEchoService*)
  let echo_service ep =
    match branch ep with
    | `True ep ->
        let x, ep = receive ep in
        let ep = send x ep in
        close ep
    | `False ep -> idle ep
  (*END*SimpleProbEchoService*)

  (*BEGIN*SimpleProbEchoClient*)
  let echo_client x ep =
    let ep = select_true ep in
    let ep = send x ep in
    let x, ep = receive ep in
    close ep;
    x
  (*END*SimpleProbEchoClient*)

  (*BEGIN*SimpleIdleClient*)
  let idle_client ep =
    let ep = select_false ep in
    idle ep
  (*END*SimpleIdleClient*)

  (*BEGIN*SimpleCoinFlipEchoClient*)
  let coin_flip_echo_client x ep =
    pick one_half
      (fun ep ->
        idle_client ep;
        x)
      (echo_client x) ep
  (*END*SimpleCoinFlipEchoClient*)
end
