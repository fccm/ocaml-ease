module Timed = ITimed.Labels

let linear_evo v = v

let print_timeelem = function
  | `At (t, v) ->
      Printf.printf " At(t:%d, v:%d)\n" t v
  | `Change (t1, t2, _, v1, v2) ->
      Printf.printf " Change(t1:%d, t2:%d, v1:%d, v2:%d)\n" 
        t1 t2 v1 v2

let print_timeline = function
  | `Static v ->
      Printf.printf "Static(v:%d)\n" v
  | `Animated anim ->
      Printf.printf "Animated [\n";
      List.iter print_timeelem anim;
      Printf.printf "]\n"


let test_static () =
  let d = 17 in
  let v = `Static d in
  let r = Timed.get_val1 ~t:120 v in
  Printf.printf "t1: %d %d\n" d r

let test_anim () =
  let tmln =
    `Animated [
      `At (0, 3);
      `Change (80, 120, linear_evo, 3, 25);
      `At (120, 27);
      `At (170, 33);
      `Change (180, 240, linear_evo, 33, 68);
    ]
  in
  print_timeline tmln;
  List.iter (fun t ->
    let r = Timed.get_val1 ~t tmln in
    Printf.printf "- t:%d => %d\n" t r
  ) [
    0; 50;
    80; 90; 100; 110; 120;
    130; 140;
    160;
    170; 175;
    180; 190; 200; 210; 220; 230; 240;
    250; 300;
  ]


let () =
  test_static ();
  test_anim ();
  ()

