module Timed = FTimed.Labels

let linear_evo v = v

let print_timeelem = function
  | `At (t, v) ->
      Printf.printf " At(t:%g, v:%g)\n" t v
  | `Change (t1, t2, _, v1, v2) ->
      Printf.printf " Change(t1:%g, t2:%g, v1:%g, v2:%g)\n" 
        t1 t2 v1 v2

let print_timeline = function
  | `Static v ->
      Printf.printf "Static(v:%g)\n" v
  | `Animated anim ->
      Printf.printf "Animated [\n";
      List.iter print_timeelem anim;
      Printf.printf "]\n"


let example_static () =
  let d = 17.0 in
  let v = `Static d in
  let r = Timed.get_val1 ~t:120.0 v in
  Printf.printf "t1: %g %g\n" d r

let example_anim () =
  let tmln =
    `Animated [
      `At (0.0, 3.0);
      `Change (80.0, 120.0, linear_evo, 3.0, 25.0);
      `At (120.0, 27.0);
      `At (170.0, 33.0);
      `Change (180.0, 240.0, linear_evo, 33.0, 68.0);
    ]
  in
  print_timeline tmln;
  List.iter (fun t ->
    let r = Timed.get_val1 ~t tmln in
    Printf.printf "- t:%g => %g\n" t r
  ) [
    0.; 50.;
    80.; 90.; 100.; 110.; 120.;
    130.; 140.;
    160.;
    170.; 175.;
    180.; 190.; 200.; 210.; 220.; 230.; 240.;
    250.; 300.;
  ]


let () =
  example_static ();
  example_anim ();
  ()

