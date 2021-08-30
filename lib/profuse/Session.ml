(* This file is part of FuSe.                                           *)
(*                                                                      *)
(* FuSe is free software: you can redistribute it and/or modify         *)
(* it under the terms of the GNU General Public License as published by *)
(* the Free Software Foundation, either version 3 of the License, or    *)
(* (at your option) any later version.                                  *)
(*                                                                      *)
(* FuSe is distributed in the hope that it will be useful,              *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of       *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *)
(* GNU General Public License for more details.                         *)
(*                                                                      *)
(* You should have received a copy of the GNU General Public License    *)
(* along with FuSe.  If not, see <http://www.gnu.org/licenses/>.        *)
(*                                                                      *)
(* Copyright 2015-2017 Luca Padovani                                    *)

exception InvalidEndpoint

module UnsafeChannel : sig
  type t

  val create : unit -> t

  val send : 'a -> t -> unit

  val receive : t -> 'a
end = struct
  type t = unit Event.channel

  let create = Event.new_channel

  let send x ch = Event.sync (Event.send ch (Obj.magic x))

  let receive ch = Obj.magic (Event.sync (Event.receive ch))
end

module Flag : sig
  type t

  val create : unit -> t

  val use : t -> unit

  val try_use : t -> bool

  val is_valid : t -> bool
end = struct
  type t = bool ref

  let create () = ref true

  let use f =
    (* BEGIN ATOMIC *)
    if !f then f := false else raise InvalidEndpoint
  (* END ATOMIC *)

  let try_use f =
    let valid = !f in
    f := false;
    valid

  let is_valid f = !f
end

type _0

type _1

type (+'a, -'b) pst = {
  name : string;
  channel : UnsafeChannel.t;
  polarity : int;
  once : Flag.t;
}

type _p_1

type _p_0

type et = (_1, _1) pst

type nt = (_0, _0) pst

type +'a it = ('a, _0) pst

type -'a ot = (_0, 'a) pst

type (+'a, +'b) choice = [ `True of 'a | `False of 'b ]

module Bare = struct
  let fresh ep = { ep with once = Flag.create () }

  (**********************************)
  (*** INITIATION AND TERMINATION ***)
  (**********************************)

  let create ?(name = "channel") () =
    let ch = UnsafeChannel.create () in
    let ep1 =
      { name = name ^ "⁺"; channel = ch; polarity = 1; once = Flag.create () }
    and ep2 =
      {
        name = name ^ "⁻";
        channel = ch;
        polarity = -1;
        once = Flag.create ();
      }
    in
    (ep1, ep2)

  let close ep = Flag.use ep.once

  let idle ep = Flag.use ep.once

  (****************)
  (*** IDENTITY ***)
  (****************)

  let same_session ep ep' = ep.channel == ep'.channel

  let string_of_endpoint ep = ep.name

  (*****************)
  (*** LINEARITY ***)
  (*****************)

  let is_valid ep = Flag.is_valid ep.once

  let acquire ep =
    Flag.use ep.once;
    fresh ep

  let try_acquire ep = if Flag.try_use ep.once then Some (fresh ep) else None

  (***********************)
  (*** MESSAGE PASSING ***)
  (***********************)

  let send x ep =
    Flag.use ep.once;
    UnsafeChannel.send x ep.channel;
    fresh ep

  let receive ep =
    Flag.use ep.once;
    (UnsafeChannel.receive ep.channel, fresh ep)

  (***************)
  (*** CHOICES ***)
  (***************)

  let select f ep =
    Flag.use ep.once;
    UnsafeChannel.send f ep.channel;
    fresh ep

  let select_true ep = select (fun x -> `True x) ep

  let select_false ep = select (fun x -> `False x) ep

  let pick fFalse fTrue ep =
    if Random.bool () then fTrue (fresh ep) else fFalse (fresh ep)

  let pick_2ch fFalse fTrue epX epY =
    if Random.bool () then fTrue (fresh epX) (fresh epY)
    else fFalse (fresh epX) (fresh epY)

  let branch ep =
    Flag.use ep.once;
    (UnsafeChannel.receive ep.channel) (fresh ep)

  let branch_2ch epX epY =
    match branch epX with
    | `True x -> `True (x, fresh epY)
    | `False x -> `False (x, fresh epY)
end
