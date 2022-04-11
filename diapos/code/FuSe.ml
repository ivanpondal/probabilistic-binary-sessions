  (*BEGIN*SumClient*)
  let sum_client ep0 x y =
    let ep1 = Session.send x ep0 in
    let ep2 = Session.send y ep1 in
    let sum, ep3 = Session.receive ep2 in
    Session.close ep3;
    sum
  (*END*SumClient*)

  (*BEGIN*SumServer*)
  let sum_server ep0 =
    let x, ep1 = Session.receive ep0 in
    let y, ep2 = Session.receive ep1 in
    let ep3 = Session.send (x + y) ep2 in
    Session.close ep3
  (*END*SumServer*)

  (*BEGIN*SumExample*)
  let epA, epB = Session.create () in
  let _ = Thread.create sum_server epA in
  sum_client epB 40 2
  (*END*SumSumExample*)
