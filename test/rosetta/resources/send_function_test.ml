open ProFuse.Session.Bare

let sender ep =
  let ep = send 42 ep in
  close ep