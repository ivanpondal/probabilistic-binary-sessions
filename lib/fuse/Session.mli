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

type (+'a, -'b) st
(** The type of endpoints for {e receiving} messages of type
['a] and {e sending} messages of type ['b]. *)

type et = (_0, _0) st
(** The type of endpoints that can only be closed. *)

type +'a it = ('a, _0) st
(** The type of endpoints for {e receiving} messages of type
['a]. *)

type -'a ot = (_0, 'a) st
(** The type of endpoints for {e sending} messages of type
['a]. *)

type (+'a, +'b) seq
(** The type of a sequentially composed protocol, where ['a] is the
protocol to be performed first, followed by ['b]. *)

type (+'a, +'b) choice = [ `True of 'a | `False of 'b ]
(** The type of a binary choice, where ['a] is the protocol to be
performed if [`True] is selected and ['b] is the protocol to be
performed if [`False] is selected. *)

module Bare : sig
  (** {2 Session initiation and termination} *)

  val create : ?name:string -> unit -> ('a, 'b) st * ('b, 'a) st
  (** [create ()] creates a new session.  @return a pair with two
valid endpoints and dual types. *)

  val close : et -> unit
  (** [close ep] closes endpoint [ep].  @raise InvalidEndpoint if the
 endpoint [ep] is invalid. *)

  (** {2 Basic message passing} *)

  val send : 'm -> ('m * ('a, 'b) st) ot -> ('b, 'a) st
  (** [send e ep] sends [e] on the endpoint [ep] with output
 capability.  @return the endpoint [ep].  @raise InvalidEndpoint if
 [ep] is invalid. *)

  val receive : ('m * ('a, 'b) st) it -> 'm * ('a, 'b) st
  (** [receive ep] receives a message from the endpoint [ep] with
 input capability.  @return a pair [(v, ep)] with the received message
 [v] and the endpoint [ep].  @raise InvalidEndpoint if the endpoint
 [ep] is invalid.  *)

  (** {2 Choices} *)

  val select : (('a, 'b) st -> 'm) -> 'm ot -> ('b, 'a) st
  (** [select f ep] sends [f] to the peer endpoint of [ep], where it
is used to compute the received message.  @return the endpoint [ep].
@raise InvalidEndpoint if the endpoint [ep] is invalid. *)

  val select_true : [> `True of ('a, 'b) st ] ot -> ('b, 'a) st
  (** [select_true ep] selects the [True] branch of a choice.  @return
 the endpoint [ep] after the selection.  @raise InvalidEndpoint if the
 endpoint [ep] is invalid. *)

  val select_false : [> `False of ('a, 'b) st ] ot -> ('b, 'a) st
  (** [select_false ep] selects the [False] branch of a choice.
 @return the endpoint [ep] after the selection.  @raise
 InvalidEndpoint if the endpoint [ep] is invalid. *)

  val branch : ([> ] as 'm) it -> 'm
  (** [branch ep] receives a selection from the endpoint [ep] with
 input capability.  @return the endpoint [ep] injected through the
 selected tag.  @raise InvalidEndpoint if the endpoint [ep] is
 invalid.  *)

  (** {2 Endpoint validity and identity} *)

  val is_valid : ('a, 'b) st -> bool
  (** [is_valid ep] determines whether [ep] is a valid endpoint or not.
 @return [true] if [ep] is valid, [false] otherwise. *)

  val acquire : ('a, 'b) st -> ('a, 'b) st
  (** [acquire ep] acquires the endpoint [ep], if it is valid.  @return
 the unique valid reference to the endpoint [ep].  @raise
 InvalidEndpoint if [ep] invalid. *)

  val try_acquire : ('a, 'b) st -> ('a, 'b) st option
  (** [try_acquire ep] attempts to acquire the endpoint [ep].  @return
 [Some ep] where [ep] is the unique valid reference to the endpoint,
 if [ep] is valid, and [None] otherwise. *)

  val same_session : ('a, 'b) st -> ('c, 'd) st -> bool
  (** [same_session ep ep'] checks whether [ep] and [ep'] are endpoints
 of the same session (but not necessarily peer endpoints).  @return
 [true] if [ep] and [ep'] are (possibly peer) endpoints pertaining the
 same session, [false] otherwise. *)

  val string_of_endpoint : ('a, 'b) st -> string
  (** [string_of_endpoint ep] returns a textual representation of the
endpoint [ep]. *)

  (** {2 Resumption combinators} *)

  val ( @= ) :
    (('a, 'b) st -> 'm * et) ->
    ((('a, 'b) st, ('c, 'd) st) seq, (('b, 'a) st, ('d, 'c) st) seq) st ->
    'm * ('c, 'd) st
  (** [f @= ep] evaluates [f ep].  @return the pair to which [f ep]
 evaluates.  @raise InvalidEndpoint if the second component of [f ep]
 is an endpoint other than [ep]. *)

  val ( @> ) :
    (('a, 'b) st -> et) ->
    ((('a, 'b) st, ('c, 'd) st) seq, (('b, 'a) st, ('d, 'c) st) seq) st ->
    ('c, 'd) st
  (** [f @> ep] evaluates [f ep].  @return the endpoint [ep].  @raise
 InvalidEndpoint if [f ep] evaluates to an endpoint different from
 [ep]. *)
end

module Monadic : sig
  type ('t0, 't1, 'a) t
  (** The type of a computation that returns a result of type ['a]
  while using a session endpoint and changing its type from ['t0] to
  ['t1]. *)

  val return : 'm -> ('t0, 't0, 'm) t
  (** [return e] is the trivial monadic computation that does not
  perform communications and returns the value of [e]. *)

  val ( >>= ) : ('t0, 't1, 'a) t -> ('a -> ('t1, 't2, 'b) t) -> ('t0, 't2, 'b) t
  (** The monadic composition operator. *)

  val ( >>> ) : ('t0, 't1, 'a) t -> ('t1, 't2, 'b) t -> ('t0, 't2, 'b) t
  (** [m1 >>> m2] is a shortcut for [m1 >>= fun _ -> m2]. *)

  val fix : (('t0, 't1, 'a) t -> ('t0, 't1, 'a) t) -> ('t0, 't1, 'a) t
  (** Fixpoint operator for monadic computations. [fix (fun x -> m)]
  represents the same computation as [m] in which [x] is bound to [m]
  itself. *)

  val connect : (('a, 'b) st, et, unit) t -> (('b, 'a) st, et, 'm) t -> 'm
  (** [connect ms mc] creates a new session that connects the server
  [ms], spawned into a new thread, and the client [mc]. The result is
  that returned by the client. *)

  val receive : (('m * ('a, 'b) st) it, ('a, 'b) st, 'm) t
  (** [receive] waits for a message from the session endpoint and
  returns its value. *)

  val send : 'm -> (('m * ('b, 'a) st) ot, ('a, 'b) st, unit) t
  (** [send e] sends the message [e] on the session endpoint. *)

  val branch :
    ('t0, 't2, 'a) t -> ('t1, 't2, 'a) t -> (('t0, 't1) choice it, 't2, 'a) t
  (** [branch mtrue mfalse] accepts a boolean selection from the
  session endpoint and executes either [mtrue] or [mfalse]
  accordingly. *)

  val select_true : ((('b, 'a) st, ('d, 'c) st) choice ot, ('a, 'b) st, unit) t
  (** [select_true] selects the [True] branch of a choice. *)

  val select_false : ((('b, 'a) st, ('d, 'c) st) choice ot, ('c, 'd) st, unit) t
  (** [select_false] selects the [False] branch of a choice. *)
end
