  (*BEGIN*AuctionBuyer*)
  let rec buyer ep offer =
    let ep = S.send offer ep in
    match S.branch ep with
    | `True ep ->
        (* `Subastador aceptó oferta *)
        S.close ep
    | `False ep ->
        (* `Subastador rechazó oferta *)
        let counteroffer, ep = receive ep in
        pick Rational.two_thirds
          (fun ep ->
            (* Comprador rechaza contraoferta *)
            let ep = S.select_false ep in
            S.idle ep)
          (fun ep ->
            (* Comprador acepta contraoferta *)
            let ep = S.select_true ep in
            buyer ep (counteroffer - 5))
          ep
  (*END*AuctionBuyer*)

  (*BEGIN*Auctioneer*)
  let rec auctioneer ep =
    let bid, ep = S.receive ep in
    pick Rational.one_quarter
      (fun ep ->
        (* Subastador rechaza oferta *)
        let ep = S.select_false ep in
        let ep = S.send (bid + 10) ep in
        match S.branch ep with
        | `True ep ->
          (* Comprador aceptó seguir contraoferta *)
          auctioneer ep
        | `False ep ->
          (* Comprador rechazó contraoferta *)
          S.idle ep)
      (fun ep ->
        (* Subastador acepta oferta *)
        let ep = S.select_true ep in
        S.close ep)
      ep
  (*END*Auctioneer*)

  (*BEGIN*CoinFlipSumServer*)
  let coin_flip_sum_server ep =
    pick one_half
      (fun ep ->
        let ep = select_false ep in
        idle ep)
      (fun ep ->
        let ep = select_true ep in
        let x, ep = receive ep in
        let y, ep = receive ep in
        let ep = send (x + y) ep in
        close ep)
      ep
  (*END*CoinFlipSumServer*)
