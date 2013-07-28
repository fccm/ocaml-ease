(* Copyright (C) 2013 Florent Monnier
 
 This software is provided "AS-IS", without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.
 
 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it freely.
*)
(* Timeline *)

(* timeline types *)

type time = int  (* in milliseconds *)

type ease_func = time -> time

type 'a animated = [
  | `At of time * 'a
  | `Change of time * time * ease_func * 'a * 'a
  ]

type 'a timed = [
  | `Static of 'a
  | `Animated of 'a animated list
  ]

(* value types *)

type t = int

type t_x1 = t
type t_x2 = t * t
type t_x3 = t * t * t
type t_x4 = t * t * t * t
type t_x5 = t * t * t * t * t
type t_x6 = t * t * t * t * t * t
type t_x7 = t * t * t * t * t * t * t
type t_x8 = t * t * t * t * t * t * t * t
type t_x9 = t * t * t * t * t * t * t * t * t

type t1 = t_x1 timed
type t2 = t_x2 timed
type t3 = t_x3 timed
type t4 = t_x4 timed
type t5 = t_x5 timed
type t6 = t_x6 timed
type t7 = t_x7 timed
type t8 = t_x8 timed
type t9 = t_x9 timed


(* interpolation functions *)

let inter1 t t1 t2 v1 v2 =
  ((v2 - v1) * (t - t1)) / (t2 - t1) + v1

let inter2 t t1 t2 (a1,b1) (a2,b2) =
  let ti = t2 - t1
  and tn = t - t1 in
  ( ((a2 - a1) * tn) / ti + a1,
    ((b2 - b1) * tn) / ti + b1 )

let inter3 t t1 t2 (a1,b1,c1) (a2,b2,c2) =
  let ti = t2 - t1
  and tn = t - t1 in
  ( ((a2 - a1) * tn) / ti + a1,
    ((b2 - b1) * tn) / ti + b1,
    ((c2 - c1) * tn) / ti + c1 )

let inter4 t t1 t2 (a1,b1,c1,d1) (a2,b2,c2,d2) =
  let ti = t2 - t1
  and tn = t - t1 in
  ( ((a2 - a1) * tn) / ti + a1,
    ((b2 - b1) * tn) / ti + b1,
    ((c2 - c1) * tn) / ti + c1,
    ((d2 - d1) * tn) / ti + d1 )

let inter5 t t1 t2 (a1,b1,c1,d1,e1) (a2,b2,c2,d2,e2) =
  let ti = t2 - t1
  and tn = t - t1 in
  ( ((a2 - a1) * tn) / ti + a1,
    ((b2 - b1) * tn) / ti + b1,
    ((c2 - c1) * tn) / ti + c1,
    ((d2 - d1) * tn) / ti + d1,
    ((e2 - e1) * tn) / ti + e1 )

let inter6 t t1 t2 (a1,b1,c1,d1,e1,f1) (a2,b2,c2,d2,e2,f2) =
  let ti = t2 - t1
  and tn = t - t1 in
  ( ((a2 - a1) * tn) / ti + a1,
    ((b2 - b1) * tn) / ti + b1,
    ((c2 - c1) * tn) / ti + c1,
    ((d2 - d1) * tn) / ti + d1,
    ((e2 - e1) * tn) / ti + e1,
    ((f2 - f1) * tn) / ti + f1 )

let inter7 t t1 t2 (a1,b1,c1,d1,e1,f1,g1) (a2,b2,c2,d2,e2,f2,g2) =
  let ti = t2 - t1
  and tn = t - t1 in
  ( ((a2 - a1) * tn) / ti + a1,
    ((b2 - b1) * tn) / ti + b1,
    ((c2 - c1) * tn) / ti + c1,
    ((d2 - d1) * tn) / ti + d1,
    ((e2 - e1) * tn) / ti + e1,
    ((f2 - f1) * tn) / ti + f1,
    ((g2 - g1) * tn) / ti + g1 )

let inter8 t t1 t2 (a1,b1,c1,d1,e1,f1,g1,h1) (a2,b2,c2,d2,e2,f2,g2,h2) =
  let ti = t2 - t1
  and tn = t - t1 in
  ( ((a2 - a1) * tn) / ti + a1,
    ((b2 - b1) * tn) / ti + b1,
    ((c2 - c1) * tn) / ti + c1,
    ((d2 - d1) * tn) / ti + d1,
    ((e2 - e1) * tn) / ti + e1,
    ((f2 - f1) * tn) / ti + f1,
    ((g2 - g1) * tn) / ti + g1,
    ((h2 - h1) * tn) / ti + h1 )

let inter9 t t1 t2 (a1,b1,c1,d1,e1,f1,g1,h1,i1) (a2,b2,c2,d2,e2,f2,g2,h2,i2) =
  let ti = t2 - t1
  and tn = t - t1 in
  ( ((a2 - a1) * tn) / ti + a1,
    ((b2 - b1) * tn) / ti + b1,
    ((c2 - c1) * tn) / ti + c1,
    ((d2 - d1) * tn) / ti + d1,
    ((e2 - e1) * tn) / ti + e1,
    ((f2 - f1) * tn) / ti + f1,
    ((g2 - g1) * tn) / ti + g1,
    ((h2 - h1) * tn) / ti + h1,
    ((i2 - i1) * tn) / ti + i1 )


(* timeline functions *)

let rec val_at inter t = function
  | `At(t1, v) :: `At(t2,_) :: _
  | `At(t1, v) :: `Change(t2,_,_,_,_) :: _
    when t1 <= t && t < t2 -> (v)
  | `At(t, v) :: [] -> (v)
  | `Change(_,t2,_,_,v2) :: []
    when t >= t2 -> (v2)
  | `Change(t1,t2,ease,v1,v2) :: _
    when t1 <= t && t <= t2 -> inter (ease t) t1 t2 v1 v2
  | _ :: tl -> val_at inter t tl
  | [] -> invalid_arg "val_at"


let get_val inter t = function
  | `Static v -> v
  | `Animated anim -> val_at inter t anim


let get_val1 t v = get_val inter1 t v
let get_val2 t v = get_val inter2 t v
let get_val3 t v = get_val inter3 t v
let get_val4 t v = get_val inter4 t v
let get_val5 t v = get_val inter5 t v
let get_val6 t v = get_val inter6 t v
let get_val7 t v = get_val inter7 t v
let get_val8 t v = get_val inter8 t v
let get_val9 t v = get_val inter9 t v


module Labels = struct

let val_at ~f ~t ~anim =
  let f t t1 t2 v1 v2 = f ~t ~t1 ~t2 ~v1 ~v2 in
  val_at f t anim

let get_val ~f ~t ~v =
  let f t t1 t2 v1 v2 = f ~t ~t1 ~t2 ~v1 ~v2 in
  get_val f t v

let get_val1 ~t v = get_val1 t v
let get_val2 ~t v = get_val2 t v
let get_val3 ~t v = get_val3 t v
let get_val4 ~t v = get_val4 t v
let get_val5 ~t v = get_val5 t v
let get_val6 ~t v = get_val6 t v
let get_val7 ~t v = get_val7 t v
let get_val8 ~t v = get_val8 t v
let get_val9 ~t v = get_val9 t v

let inter1 ~t ~t1 ~t2 ~v1 ~v2 = inter1 t t1 t2 v1 v2
let inter2 ~t ~t1 ~t2 ~v1 ~v2 = inter2 t t1 t2 v1 v2
let inter3 ~t ~t1 ~t2 ~v1 ~v2 = inter3 t t1 t2 v1 v2
let inter4 ~t ~t1 ~t2 ~v1 ~v2 = inter4 t t1 t2 v1 v2
let inter5 ~t ~t1 ~t2 ~v1 ~v2 = inter5 t t1 t2 v1 v2
let inter6 ~t ~t1 ~t2 ~v1 ~v2 = inter6 t t1 t2 v1 v2
let inter7 ~t ~t1 ~t2 ~v1 ~v2 = inter7 t t1 t2 v1 v2
let inter8 ~t ~t1 ~t2 ~v1 ~v2 = inter8 t t1 t2 v1 v2
let inter9 ~t ~t1 ~t2 ~v1 ~v2 = inter9 t t1 t2 v1 v2

end
