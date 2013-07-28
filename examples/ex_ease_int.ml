let fs = [|
  "`Linear",  `Linear;

  "`QuadIn",  `QuadIn;
  "`CubicIn", `CubicIn;
  "`QuartIn", `QuartIn;

  "`QuadOut",  `QuadOut;
  "`CubicOut", `CubicOut;
  "`QuartOut", `QuartOut;

  "`QuadInOut",  `QuadInOut;
  "`CubicInOut", `CubicInOut;
  "`QuartInOut", `QuartInOut;
|]

let border = 20
let size = 220
let b_size = size + (2 * border)
let f_size = float size

let () =
  Graphics.open_graph "";
  Graphics.resize_window b_size b_size;
  let fi = ref 0 in
  let n = Array.length fs in
  while true do
    let name, f = 
      let name, f = fs.(!fi) in
      let f = IEase.f f in
      (name, f)
    in
    Graphics.clear_graph ();
    Graphics.draw_rect border border size size;
    Graphics.moveto 6 (b_size - 16);
    Graphics.draw_string name;
    for i = 0 to pred (size * 2) do
      let x = (float i) /. 2.0 /. f_size in
      let _x = int_of_float (x *. 1000.0) in
      let y = f _x in
      let y = (float_of_int y) /. 1000.0 in
      let coord v = truncate (v *. f_size) + border in
      let x = coord x in
      let y = coord y in
      Graphics.plot x y
    done;
    let k = Graphics.read_key () in
    if k = '\027' then exit 0;
    fi := (1 + !fi) mod n
  done
