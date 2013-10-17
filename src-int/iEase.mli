(* Copyright (C) 2013 Florent Monnier
 
 This software is provided "AS-IS", without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.
 
 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it freely.
*)
(** Easing *)

(** The interval for calculations is not (0.0 .. 1.0) as usual,
    but (0 .. 1000) because we're using integers here. *)

(** {3 Linear} *)

val linear : int -> int

(** {3 Quadratic} *)

val quad_in : int -> int
val quad_out : int -> int
val quad_inOut : int -> int

(** {3 Cubic} *)

val cubic_in : int -> int
val cubic_out : int -> int
val cubic_inOut : int -> int

(** {3 Quartic} *)

val quart_in : int -> int
val quart_out : int -> int
val quart_inOut : int -> int


(** {3 All} *)

type ease = [
  | `Linear
  | `QuadIn
  | `QuadOut
  | `QuadInOut
  | `CubicIn
  | `CubicOut
  | `CubicInOut
  | `QuartIn
  | `QuartOut
  | `QuartInOut
  ]

val f : ease -> int -> int

type ease_func = int -> int
val get_func : ease -> ease_func


(** {3 String-able} *)

val to_string : ease -> string
val of_string : string -> ease

