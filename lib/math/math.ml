module Natural = struct
  type _z

  type _s

  type _ nat = Z : _z nat | S : 'n nat -> (_s * 'n) nat

  type 'a suc = _s * 'a

  type zero = _z

  let rec nat_to_int : type a. a nat -> int = function
    | Z -> 0
    | S n -> 1 + nat_to_int n

  let one = S Z

  let two = S one

  let three = S two

  let four = S three
end

open Natural

module Rational = struct
  type _ frac = Fraction : 'n nat * 'd suc nat -> ('n nat * 'd suc nat) frac

  let frac_to_float : _ frac -> float = function
    | Fraction (n, d) ->
        float_of_int (nat_to_int n) /. float_of_int (nat_to_int d)

  let one_half = Fraction (one, two)

  let one_quarter = Fraction (one, four)

  let one_third = Fraction (one, three)

  let two_thirds = Fraction (two, three)
end

open Owl
open Owl.Mat

module Markov = struct
  let absortion_matrix q r =
    let id = eye (row_num q) in
    let n = Mat.( - ) id q |> inv in
    dot n r

end
