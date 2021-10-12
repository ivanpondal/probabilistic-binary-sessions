open ProFuse.Session.Bare

let receiver ep =
  let _, ep = receive ep in
  close ep