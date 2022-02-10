open ProFuse.Session.Bare

let inversion epX epY =
  match branch_2ch epX epY with
  | `True (epX, epY) -> (
      let epY = select_false epY in
      close epY;
      match branch epX with `True epX -> close epX | `False epX -> idle epX)
  | `False (epX, epY) -> (
      let epY = select_true epY in
      idle epY;
      match branch epX with `True epX -> close epX | `False epX -> idle epX)
