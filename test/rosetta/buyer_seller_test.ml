open ProFuse.Session.Bare
open Math.Rational

let rec seller ep =
  let bid, ep = receive ep in
  pick one_quarter
    (fun ep ->
      let ep = select_false ep in
      let ep = send (bid + 10) ep in
      match branch ep with `True ep -> idle ep | `False ep -> seller ep)
    (fun ep ->
      let ep = select_true ep in
      close ep)
    ep

let rec buyer ep offer =
  let ep = send offer ep in
  match branch ep with
  | `True ep ->
      close ep;
      offer
  | `False ep ->
      let counteroffer, ep = receive ep in
      pick two_thirds
        (fun ep ->
          let ep = select_false ep in
          buyer ep counteroffer)
        (fun ep ->
          let ep = select_true ep in
          idle ep;
          -1)
        ep

let test_buyer_seller ?(st = cst_placeholder) () =
  let ep1, ep2 = create ~st () in
  let _ = Thread.create seller ep1 in
  buyer ep2 42
