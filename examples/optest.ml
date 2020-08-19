open Op

let s =
  let+ lfo = sine in
  let+ osc = saw in
  let s = osc (cst 440.) |> amp (add (lfo (cst 10.)) (cst 0.1)) |> stereo in
  return s

let () =
  Output.play s
