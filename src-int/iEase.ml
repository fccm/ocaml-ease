(* Copyright (C) 2013 Florent Monnier
 
 This software is provided "AS-IS", without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.
 
 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it freely.
*)
(* Easing *)

let linear t = t

let quad_in t = (t * t) / 1000
let quad_out t = (t * (2000 - t)) / 1000

let quad_inOut t =
  if t < 500 then (2 * t * t) / 1000
  else ((4000 - 2 * t) * t - 1000_000) / 1000

let cubic_in t = (t * t * t) / 1000_000
let cubic_out t =
  let t = t - 1000 in
  (((t * t * t) / 1000_000) + 1000)

let cubic_inOut t =
  if t < 500 then (4 * ((t * t * t) / 1000)) / 1000
  else
    let t = t * 2 in
    let t = t - 2000 in
    (((t * t * t) / 1000_000) + 2000) / 2

(* do not divide by 1000_000_000 : max_int overflow *)
let quart_in t = (((t * t * t) / 1000_000) * t) / 1000

let quart_out t =
  let t = t - 1000 in
  1000 - (quart_in t)

let quart_inOut t =
  let t = t * 2 in
  if t < 1000
  then (quart_in t) / 2
  else let t = t - 2000 in
    ((quart_in t) - 2000) / (-2)

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

type ease_func = int -> int

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

