module Echo = struct
  (*BEGIN*SimpleEchoClient*)
  let echo_client ep x =
    let ep = Session.send x ep in
    let res, ep = Session.receive ep in
    Session.close ep;
    res
  (*END*SimpleEchoClient*)

  (*BEGIN*SimpleEchoService*)
  let echo_service ep =
    let x, ep = Session.receive ep in
    let ep = Session.send x ep in
    Session.close ep
  (*END*SimpleEchoService*)

  (*BEGIN*SimpleEchoMain*)
  let _ =
    let a, b = Session.create () in
    let _ = Thread.create echo_service a in
    print_endline (echo_client b "Hola mundo")
  (*END*SimpleEchoMain*)

  (*BEGIN*OptionalEchoService*)
  let opt_echo_service ep =
    match Session.branch ep with
    | `Msg ep -> echo_service ep
    | `End ep -> Session.close ep
  (*END*OptionalEchoService*)

  (*BEGIN*OptionalEchoClient*)
  let opt_echo_client ep opt x =
    if opt then
      let ep = Session.select (fun x -> `Msg x) ep
      in echo_client ep x
    else
      let ep = Session.select (fun x -> `End x) ep
      in Session.close ep; x
  (*END*OptionalEchoClient*)

  (*BEGIN*EtaExpansion*)
  let _Msg x = `Msg x
  let _End x = `End x
  (*END*EtaExpansion*)

  (*BEGIN*RecursiveEchoService*)
  let rec rec_echo_service ep =
    match Session.branch ep with
    | `Msg ep -> let x, ep = Session.receive ep in
                let ep = Session.send x ep in
                rec_echo_service ep
    | `End ep -> Session.close ep
  (*END*RecursiveEchoService*)

  (*BEGIN*RecursiveEchoClient*)
  let rec rec_echo_client ep =
    function
    | [] -> let ep = Session.select _End ep in
           Session.close ep; []
    | x :: xs -> let ep = Session.select _Msg ep in
                let ep = Session.send x ep in
                let y, ep = Session.receive ep in
                y :: rec_echo_client ep xs
  (*END*RecursiveEchoClient*)
end
