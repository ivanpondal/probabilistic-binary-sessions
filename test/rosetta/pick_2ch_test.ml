open ProFuse.Session.Bare
open Math.Rational

let pick_two_channels epX epY =
  pick_2ch one_half
    (fun epX epY ->
      let epX = select_false epX in
      idle epX;
      let epY = select_false epY in
      close epY)
    (fun epX epY ->
      let epX = select_true epX in
      close epX;
      let epY = select_true epY in
      close epY)
    epX epY
