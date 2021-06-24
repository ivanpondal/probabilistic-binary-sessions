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

module S = Fuse.Session.Bare

let echo_client ep x =
  let ep = S.send x ep in
  let res, ep = S.receive ep in
  S.close ep;
  res

let echo_service ep =
  let x, ep = S.receive ep in
  let ep = S.send x ep in
  S.close ep

let _ =
  let a, b = S.create () in
  let _ = Thread.create echo_service a in
  print_string (echo_client b "Hello")
