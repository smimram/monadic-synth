(** R2D2-ish sound. *)

open Msynth
open Stream

let () =
  let s =
    let freq = random () ~min:500. ~max:1500. 10. in
    let osc  = sine () in
    freq >>= osc >>= stereo
  in
  Output.play s
