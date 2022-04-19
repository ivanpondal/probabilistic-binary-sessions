  (*BEGIN*SumClient*)
  let sum_client ep0 x y =
    let ep1 = S.send x ep0 in
    let ep2 = S.send y ep1 in
    let sum, ep3 = S.receive ep2 in
    S.close ep3;
    sum
  (*END*SumClient*)

  (*BEGIN*SumServer*)
  let sum_server ep0 =
    let x, ep1 = S.receive ep0 in
    let y, ep2 = S.receive ep1 in
    let ep3 = S.send (x + y) ep2 in
    S.close ep3
  (*END*SumServer*)

  (*BEGIN*SumExample*)
  let epA, epB = S.create () in
  let _ = Thread.create sum_server epA in
  sum_client epB 40 2
  (*END*SumSumExample*)

  (*BEGIN*SumServerRec*)
  let rec sum_server_rec ep0 =
    let x, ep1 = S.receive ep0 in
    let y, ep2 = S.receive ep1 in
    let ep3 = S.send (x + y) ep2 in
    match S.branch ep3 with
    | `True ep4 -> sum_server_rec ep4
    | `False ep4 -> close ep4
  (*END*SumServerRec*)

  (*END*SumThreeNumClient*)
  let sum_three_num_client ep0 x y z =
    (* Primer suma *)
    let ep1 = S.send x ep0 in
    let ep2 = S.send y ep1 in
    let first_sum, ep3 = S.receive ep2 in
    let ep4 = S.select_true ep3 in
    (* Segunda suma *)
    let ep5 = S.send first_sum ep4 in
    let ep6 = S.send z ep5 in
    let sum, ep7 = S.receive ep6 in
    let ep8 = S.select_false ep7 in
    S.close ep8;
    sum
  (*END*SumThreeNumClient*)
