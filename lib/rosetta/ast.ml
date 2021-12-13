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
(* Copyright 2015-2016 Luca Padovani                                    *)

type t =
  | Var of string
  | RecVar of string
  | SessionTypeVar of string * string
  | Rec of string * t
  | Tagged of tagged_constructor_t * (string * t) list
  | Constructor of constructor_t * t list

and constructor_t =
  [ `As of string
  | `Empty
  | `End
  | `Done
  | `Prob of float
  | `ConvexSum
  | `Nat of int
  | `Frac of float
  | `Arrow
  | `Tuple
  | `Channel
  | `ClosedChannel
  | `Apply of string list
  | `Send
  | `Receive
  | `Sequence
  | `SelectSequence
  | `AcceptSequence ]

and tagged_constructor_t = [ `Variant | `Choice | `Branch ]

let priority_of_constructor = function
  | `Empty | `End | `Done | `Nat _ | `Frac _ | `Prob _ | `ConvexSum | `Channel
  | `ClosedChannel | `Apply _ ->
      5
  | `As _ -> 4
  | `Send | `Receive | `Sequence | `SelectSequence | `AcceptSequence -> 3
  | `Arrow -> 2
  | `Tuple -> 1

let rec priority_of_type = function
  | Var _ | RecVar _ | SessionTypeVar _ | Tagged _ -> 5
  | Rec (_, t) -> priority_of_type t
  | Constructor (ctor, _) -> priority_of_constructor ctor

let t_End = Constructor (`End, [])

let t_Done = Constructor (`Done, [])

let t_Empty = Constructor (`Empty, [])

let t_Nat n = Constructor (`Nat n, [])

let t_Frac f = Constructor (`Frac f, [])

let t_Prob p = Constructor (`Prob p, [])

let t_ConvexSum p q r = Constructor (`ConvexSum, [ p; q; r ])
