(* Copyright (C) 2013 Florent Monnier
 
 This software is provided "AS-IS", without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.
 
 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it freely.
*)
(* Easing *)

let linear t = t

let quad_in t = t *. t
let quad_out t = t *. (2.0 -. t)
let quad_inOut t =
  if t < 0.5 then 2.0 *. t *. t
  else (4.0 -. 2.0 *. t) *. t -. 1.0

let cubic_in t = t *. t *. t
let cubic_out t =
  let t = t -. 1.0 in
  t *. t *. t +. 1.0

let cubic_inOut t =
  if t < 0.5 then 4.0 *. t *. t *. t
  else
    let t = t *. 2.0 in
    let t = t -. 2.0 in
    (t *. t *. t +. 2.0) *. 0.5

let quart_in t = t *. t *. t *. t
let quart_out t =
  let t = t -. 1.0 in
  1.0 -. t *. t *. t *. t

let quart_inOut t =
  let t = t *. 2.0 in
  if 1.0 > t
  then 0.5 *. t *. t *. t *. t
  else let t = t -. 2.0 in
    -0.5 *. (t *. t *. t *. t -. 2.0)

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

let f e t =
  match e with
  | `Linear     -> linear t
  | `QuadIn     -> quad_in t
  | `QuadOut    -> quad_out t
  | `QuadInOut  -> quad_inOut t
  | `CubicIn    -> cubic_in t
  | `CubicOut   -> cubic_out t
  | `CubicInOut -> cubic_inOut t
  | `QuartIn    -> quart_in t
  | `QuartOut   -> quart_out t
  | `QuartInOut -> quart_inOut t

type ease_func = float -> float

let get_func = function
  | `Linear     -> linear
  | `QuadIn     -> quad_in
  | `QuadOut    -> quad_out
  | `QuadInOut  -> quad_inOut
  | `CubicIn    -> cubic_in
  | `CubicOut   -> cubic_out
  | `CubicInOut -> cubic_inOut
  | `QuartIn    -> quart_in
  | `QuartOut   -> quart_out
  | `QuartInOut -> quart_inOut

let to_string = function
  | `Linear     -> "Linear"
  | `QuadIn     -> "QuadIn"
  | `QuadOut    -> "QuadOut"
  | `QuadInOut  -> "QuadInOut"
  | `CubicIn    -> "CubicIn"
  | `CubicOut   -> "CubicOut"
  | `CubicInOut -> "CubicInOut"
  | `QuartIn    -> "QuartIn"
  | `QuartOut   -> "QuartOut"
  | `QuartInOut -> "QuartInOut"

let of_string s =
  match String.lowercase s with
  | "linear"      -> `Linear
  | "quadin"      -> `QuadIn
  | "quadout"     -> `QuadOut
  | "quadinout"   -> `QuadInOut
  | "cubicin"     -> `CubicIn
  | "cubicout"    -> `CubicOut
  | "cubicinout"  -> `CubicInOut
  | "quartin"     -> `QuartIn
  | "quartout"    -> `QuartOut
  | "quartinout"  -> `QuartInOut
  | _ -> invalid_arg "of_string"

