open ProFuse.Session.Bare
open Math.Rational

let pick_single_channel ep =
  pick one_half
    (fun ep ->
      let ep = select_false ep in
      idle ep)
    (fun ep ->
      let ep = select_true ep in
      close ep)
    ep
