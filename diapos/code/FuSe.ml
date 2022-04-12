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

  (*BEGIN*SumServerRec*)
  let rec sum_server_rec ep0 =
    let x, ep1 = Session.receive ep0 in
    let y, ep2 = Session.receive ep1 in
    let ep3 = Session.send (x + y) ep2 in
    match Session.branch ep3 with
    | `True ep4 -> sum_server_rec ep4
    | `False ep4 -> close ep4
  (*END*SumServerRec*)

  (*END*SumThreeNumClient*)
  let sum_three_num_client ep0 x y z =
    let ep1 = Session.send x ep0 in
    let ep2 = Session.send y ep1 in
    let first_sum, ep3 = Session.receive ep2 in
    let ep4 = Session.select_true ep3 in
    let ep5 = Session.send first_sum ep4 in
    let ep6 = Session.send z ep5 in
    let sum, ep7 = Session.receive ep6 in
    let ep8 = Session.select_false ep7 in
    Session.close ep8;
    sum
  (*END*SumThreeNumClient*)
