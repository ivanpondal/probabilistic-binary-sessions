open ProFuse.Session.Bare
let select_true ep =
  let ep = select_true ep in
  close ep
