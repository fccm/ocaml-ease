(* Copyright (C) 2013 Florent Monnier
 
 This software is provided "AS-IS", without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.
 
 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it freely.
*)
(** Easing *)

(** {3 Linear} *)

val linear : float -> float

(** {3 Quadratic} *)

val quad_in : float -> float
val quad_out : float -> float
val quad_inOut : float -> float

(** {3 Cubic} *)

val cubic_in : float -> float
val cubic_out : float -> float
val cubic_inOut : float -> float

(** {3 Quartic} *)

val quart_in : float -> float
val quart_out : float -> float
val quart_inOut : float -> float


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

val f : ease -> float -> float

type ease_func = float -> float
val get_func : ease -> ease_func


(** {3 String-able} *)

val to_string : ease -> string
val of_string : string -> ease

