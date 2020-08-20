(** The THX deep note. *)

(* We are more or less following the explanations from
   http://earslap.com/article/recreating-the-thx-deep-note.html *)

open Extlib
open Stream

let () =
  (* Number of voices. *)
  let voices = 30 in
  let voice () =
    (* Initial frequency. *)
    let freq = Random.float 200. +. 200. in
    (* Final frequency. *)
    let final = Note.frequency (Float.floor (Random.float 7.) *. 12. +. 2.5) in
    (* Add some noise in final frequency. *)
    let final =
      let e = 0.01 in
      let min = final *. (1. -. e) in
      let max = final *. (1. +. e) in
      random () ~min ~max 0.5 >>= smooth () 1.
    in
    let freq =
      let ramp = Envelope.ramp () in
      let* target = final in
      ramp ~from:freq ~target 5. in
    freq
    >>= saw ()
    >>= amp (1. /. float voices)
    >>= Stereo.pan ~law:`Circular (Random.float 2. -. 1.)
  in
  let voices = List.init voices (fun _ -> voice ()) in
  let s =
    Stereo.mix voices
    (* Avoid abrupt start *)
    >>= Stereo.Envelope.apply (Envelope.ramp () 0.1)
    (* Adjust general volume *)
    >>= Stereo.amp 0.5
  in
  Output.play s
