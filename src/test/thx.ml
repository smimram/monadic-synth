(** The THX deep note. *)

(* http://earslap.com/article/recreating-the-thx-deep-note.html *)

open Extlib
open Stream

let () =
  (* Number of voices. *)
  let voices = 30 in
  let voice () =
    (* Initial frequency. *)
    let freq = Random.float 200. +. 200. in
    (* Final frequency. *)
    let final = Note.frequency (Float.floor (Random.float 6.) *. 12. +. 14.5) in
    (* Add some noise in final frequency. *)
    let final =
      let e = 0.01 in
      let min = final *. (1. -. e) in
      let max = final *. (1. +. e) in
      random () ~min ~max 0.5 >>= smooth () 1.
    in
    let freq = bind2 (Envelope.ramp () freq) final (return 4.) in
    freq
    >>= saw ()
    >>= amp (1. /. float voices)
    >>= Stereo.pan ~law:`Circular () (Random.float 2. -. 1.)
  in
  let voices = List.init voices (fun _ -> voice ()) in
  Output.play (Stereo.mix voices >>= Stereo.amp 0.5)
