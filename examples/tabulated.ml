(** Test tabulated functions. *)

open Msynth
open Stream

let () =
  let s =
    let freq = 440. in
    let s  = sine () freq in
    let s' = sine_tabulated () freq in
    (* Switch between the two oscillators every 1 second. *)
    let b = ref true in
    let e = every () 1. >>= on (fun () -> b := not !b) in
    let b = e >> stream_ref b in
    b >>= switch s s' >>= stereo
  in
  Output.play s
