open Op

let s =
  let+ osc = saw in
  let s = osc (cst 440.) |> stereo in
  return s

let () =
  Output.play s
