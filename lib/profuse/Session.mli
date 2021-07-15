(** OCaml implementation of binary sessions.
 @author Luca Padovani and Hernán Melgratti
 @version 0.7
 *)

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
(** Exception raised whenever an invalid endpoint is used. *)

type _0
(** Empty type. *)

type _p_1

type _p_0

type (+'a, -'b, 'p) pst
(** The type of endpoints for {e receiving} messages of type
['a] and {e sending} messages of type ['b]. *)

type et = (_0, _0, _p_1) pst
(** The type of endpoints that can only be closed. *)

type nt = (_0, _0, _p_0) pst

type (+'a, 'p) it = ('a, _0, 'p) pst
(** The type of endpoints for {e receiving} messages of type
['a]. *)

type (-'a, 'p) ot = (_0, 'a, 'p) pst
(** The type of endpoints for {e sending} messages of type
['a]. *)

type ('p, 'q, 'r) pchoice

module Bare : sig
  (** {2 Session initiation and termination} *)

  val create : ?name:string -> unit -> ('a, 'b, 'p) pst * ('b, 'a, 'p) pst
  (** [create ()] creates a new session.  @return a pair with two
valid endpoints and dual types. *)

  val close : et -> unit
  (** [close ep] closes endpoint [ep].  @raise InvalidEndpoint if the
 endpoint [ep] is invalid. *)

  val idle : nt -> unit

  (** {2 Basic message passing} *)

  val send : 'm -> ('m * ('a, 'b, 'q) pst, 'p) ot -> ('b, 'a, 'q) pst
  (** [send e ep] sends [e] on the endpoint [ep] with output
 capability.  @return the endpoint [ep].  @raise InvalidEndpoint if
 [ep] is invalid. *)

  val receive : ('m * ('a, 'b, 'q) pst, 'p) it -> 'm * ('a, 'b, 'q) pst
  (** [receive ep] receives a message from the endpoint [ep] with
 input capability.  @return a pair [(v, ep)] with the received message
 [v] and the endpoint [ep].  @raise InvalidEndpoint if the endpoint
 [ep] is invalid.  *)

  (** {2 Choices} *)

  val select : (('a, 'b, 'p) pst -> 'm) -> ('m, 'q) ot -> ('b, 'a, 'p) pst
  (** [select f ep] sends [f] to the peer endpoint of [ep], where it
is used to compute the received message.  @return the endpoint [ep].
@raise InvalidEndpoint if the endpoint [ep] is invalid. *)

  val select_true :
    ([> `True of ('a, 'b, 'q) pst ] * _p_0, 'r) ot -> ('b, 'a, 'q) pst
  (** [select_true ep] selects the [True] branch of a choice.  @return
 the endpoint [ep] after the selection.  @raise InvalidEndpoint if the
 endpoint [ep] is invalid. *)

  val select_false :
    ([> `False of ('a, 'b, 'p) pst ] * _p_1, 'r) ot -> ('b, 'a, 'p) pst
  (** [select_false ep] selects the [False] branch of a choice.
 @return the endpoint [ep] after the selection.  @raise
 InvalidEndpoint if the endpoint [ep] is invalid. *)

  val pick :
    ((([> `False of ('a, 'b, 's) pst | `True of ('c, 'd, 't) pst ] as 'm) * 'q,
       'u )
     ot ->
    'e) ->
    (('m * 'r, 'v) ot -> 'e) ->
    ('m * ('p, 'q, 'r) pchoice, 'w) ot ->
    'e

  val branch : (([> ] as 'm) * 'p, 'q) it -> 'm
  (** [branch ep] receives a selection from the endpoint [ep] with
 input capability.  @return the endpoint [ep] injected through the
 selected tag.  @raise InvalidEndpoint if the endpoint [ep] is
 invalid.  *)

  (** {2 Endpoint validity and identity} *)

  val is_valid : ('a, 'b, 'p) pst -> bool
  (** [is_valid ep] determines whether [ep] is a valid endpoint or not.
 @return [true] if [ep] is valid, [false] otherwise. *)

  val acquire : ('a, 'b, 'p) pst -> ('a, 'b, 'p) pst
  (** [acquire ep] acquires the endpoint [ep], if it is valid.  @return
 the unique valid reference to the endpoint [ep].  @raise
 InvalidEndpoint if [ep] invalid. *)

  val try_acquire : ('a, 'b, 'p) pst -> ('a, 'b, 'p) pst option
  (** [try_acquire ep] attempts to acquire the endpoint [ep].  @return
 [Some ep] where [ep] is the unique valid reference to the endpoint,
 if [ep] is valid, and [None] otherwise. *)

  val same_session : ('a, 'b, 'p) pst -> ('c, 'd, 'p) pst -> bool
  (** [same_session ep ep'] checks whether [ep] and [ep'] are endpoints
 of the same session (but not necessarily peer endpoints).  @return
 [true] if [ep] and [ep'] are (possibly peer) endpoints pertaining the
 same session, [false] otherwise. *)

  val string_of_endpoint : ('a, 'b, 'p) pst -> string
  (** [string_of_endpoint ep] returns a textual representation of the
endpoint [ep]. *)
end
