open Msynth
open Stream

let s =
  let n1 = Osc.noise () in
  let n2 = Osc.pink_noise () in
  let b = alternately () in
  b 0.5 >>= switch n1 n2 >>= stereo

let () =
  Output.play s
