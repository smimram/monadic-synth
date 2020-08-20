(* A simple theremin. *)

open Stream

let () =
  Graphics.open_graph "";
  let pos = seq Graphics.mouse_pos in
  let pos = downsample 1000. pos in
  let vol =
    let* x, _ = pos in
    let x = float x /. float (Graphics.size_x ()) in
    return x
  in
  let freq =
    let* _, y = pos in
    let y = float y /. float (Graphics.size_y ()) in
    return (Math.stretch ~min:300. ~max:2500. y)
  in
  let s = bind (sine ()) freq >>= B.mulc vol in
  Output.play (s >>= amp 0.5 >>= stereo)
