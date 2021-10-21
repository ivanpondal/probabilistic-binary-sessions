module Natural : sig
  type _z

  type _s

  type _ nat = Z : _z nat | S : 'n nat -> (_s * 'n) nat

  type 'a suc = _s * 'a

  type zero = _z

  val nat_to_int : _ nat -> int

  val one : zero suc nat

  val two : zero suc suc nat

  val three : zero suc suc suc nat

  val four : zero suc suc suc suc nat
end

open Natural

module Rational : sig
  type _ frac = Fraction : 'n nat * 'd suc nat -> ('n nat * 'd suc nat) frac

  val frac_to_float : ('n nat * 'd suc nat) frac -> float

  val one_half : (zero suc nat * zero suc suc nat) frac

  val one_quarter : (zero suc nat * zero suc suc suc suc nat) frac

  val two_thirds : (zero suc suc nat * zero suc suc suc nat) frac
end

module Markov : sig
  val absortion_matrix :
    Owl_linalg_d.mat -> Owl_linalg_d.mat -> Owl_linalg_d.mat
  (** [absortion_matrix q r] calculates absortion matrix [b] = (I - [q])^(-1) x [r].
  @return the absortion matrix [b]. *)
end
