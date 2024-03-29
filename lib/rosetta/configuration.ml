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

let show_end = ref false

let sequence_polarity = ref false

let prefix = ref [ "ProFuse__Session" ]

let session_type = ref "pst"

let get_prefix () = !prefix

let set_prefix s = prefix := s

let reset_prefix () = prefix := []

let math_prefix = ref [ "Math" ]

let get_natural_prefix = List.append !math_prefix [ "Natural" ]

let get_rational_prefix = List.append !math_prefix [ "Rational" ]
