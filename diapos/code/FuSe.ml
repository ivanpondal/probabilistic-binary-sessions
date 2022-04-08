  (*BEGIN*SumClient*)
  let sum_client ep x y =
    let ep = Session.send x ep in
    let ep = Session.send y ep in
    let sum, ep = Session.receive ep in
    Session.close ep;
    sum
  (*END*SumClient*)

  (*BEGIN*SumServer*)
  let sum_server ep =
    let x, ep = Session.receive ep in
    let y, ep = Session.receive ep in
    let ep = Session.send (x + y) ep in
    Session.close ep
  (*END*SumServer*)

  (*BEGIN*SumExample*)
  let ep1, ep2 = Session.create () in
  let _ = Thread.create sum_server ep1 in
  sum_client ep2 40 2
  (*END*SumSumExample*)
