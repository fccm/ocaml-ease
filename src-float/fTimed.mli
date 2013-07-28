(* Copyright (C) 2013 Florent Monnier
 
 This software is provided "AS-IS", without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.
 
 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it freely.
*)
(** Timeline *)

(** {3 Timeline types} *)

type time = float

type ease_func = time -> time

type 'a animated = [
  | `At of time * 'a
  | `Change of time * time * ease_func * 'a * 'a
  ]

type 'a timed = [
  | `Static of 'a
  | `Animated of 'a animated list
  ]

(** {3 Value types} *)

type t = float

type t_x1 = t
type t_x2 = t * t
type t_x3 = t * t * t
type t_x4 = t * t * t * t
type t_x5 = t * t * t * t * t
type t_x6 = t * t * t * t * t * t
type t_x7 = t * t * t * t * t * t * t
type t_x8 = t * t * t * t * t * t * t * t
type t_x9 = t * t * t * t * t * t * t * t * t
type t_xa = t array

type t1 = t_x1 timed
type t2 = t_x2 timed
type t3 = t_x3 timed
type t4 = t_x4 timed
type t5 = t_x5 timed
type t6 = t_x6 timed
type t7 = t_x7 timed
type t8 = t_x8 timed
type t9 = t_x9 timed
type ta = t_xa timed


(** {3 Interpolation functions} *)

val inter1 :
  time -> time -> time ->
  t_x1 -> t_x1 -> t_x1

val inter2 :
  time -> time -> time ->
  t_x2 -> t_x2 -> t_x2

val inter3 :
  time -> time -> time ->
  t_x3 -> t_x3 -> t_x3

val inter4 :
  time -> time -> time ->
  t_x4 -> t_x4 -> t_x4

val inter5 :
  time -> time -> time ->
  t_x5 -> t_x5 -> t_x5

val inter6 :
  time -> time -> time ->
  t_x6 -> t_x6 -> t_x6

val inter7 :
  time -> time -> time ->
  t_x7 -> t_x7 -> t_x7

val inter8 :
  time -> time -> time ->
  t_x8 -> t_x8 -> t_x8

val inter9 :
  time -> time -> time ->
  t_x9 -> t_x9 -> t_x9

val intera :
  time -> time -> time ->
  t_xa -> t_xa -> t_xa


(** {3 Timeline functions} *)

val val_at :
  (time -> time -> time -> 'a -> 'a -> 'a) ->
    time -> 'a animated list -> 'a

val get_val :
  (time -> time -> time -> 'a -> 'a -> 'a) ->
    time -> 'a timed -> 'a

val get_val1 : time -> t1 -> t_x1
val get_val2 : time -> t2 -> t_x2
val get_val3 : time -> t3 -> t_x3
val get_val4 : time -> t4 -> t_x4
val get_val5 : time -> t5 -> t_x5
val get_val6 : time -> t6 -> t_x6
val get_val7 : time -> t7 -> t_x7
val get_val8 : time -> t8 -> t_x8
val get_val9 : time -> t9 -> t_x9
val get_vala : time -> ta -> t_xa


(** {3 Labeled functions} *)

module Labels : sig

(** {3 Timeline functions} *)

val val_at :
  f:(t:time -> t1:time -> t2:time -> v1:'a -> v2:'a -> 'a) ->
  t:time -> anim:'a animated list -> 'a

val get_val :
  f:(t:time -> t1:time -> t2:time -> v1:'a -> v2:'a -> 'a) ->
  t:time -> v:'a timed -> 'a

val get_val1 : t:time -> t1 -> t_x1
val get_val2 : t:time -> t2 -> t_x2
val get_val3 : t:time -> t3 -> t_x3
val get_val4 : t:time -> t4 -> t_x4
val get_val5 : t:time -> t5 -> t_x5
val get_val6 : t:time -> t6 -> t_x6
val get_val7 : t:time -> t7 -> t_x7
val get_val8 : t:time -> t8 -> t_x8
val get_val9 : t:time -> t9 -> t_x9
val get_vala : t:time -> ta -> t_xa


(** {3 Interpolation functions} *)

val inter1 :
  t:time -> t1:time -> t2:time ->
  v1:t_x1 -> v2:t_x1 -> t_x1

val inter2 :
  t:time -> t1:time -> t2:time ->
  v1:t_x2 -> v2:t_x2 -> t_x2

val inter3 :
  t:time -> t1:time -> t2:time ->
  v1:t_x3 -> v2:t_x3 -> t_x3

val inter4 :
  t:time -> t1:time -> t2:time ->
  v1:t_x4 -> v2:t_x4 -> t_x4

val inter5 :
  t:time -> t1:time -> t2:time ->
  v1:t_x5 -> v2:t_x5 -> t_x5

val inter6 :
  t:time -> t1:time -> t2:time ->
  v1:t_x6 -> v2:t_x6 -> t_x6

val inter7 :
  t:time -> t1:time -> t2:time ->
  v1:t_x7 -> v2:t_x7 -> t_x7

val inter8 :
  t:time -> t1:time -> t2:time ->
  v1:t_x8 -> v2:t_x8 -> t_x8

val inter9 :
  t:time -> t1:time -> t2:time ->
  v1:t_x9 -> v2:t_x9 -> t_x9

val intera :
  t:time -> t1:time -> t2:time ->
  v1:t_xa -> v2:t_xa -> t_xa

end
