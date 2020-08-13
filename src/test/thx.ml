(** The THX deep note. *)

(* http://earslap.com/article/recreating-the-thx-deep-note.html *)

open Stream

let () =
  (* Number of voices. *)
  let voices = 30 in
  let voice () =
    (* Initial frequency. *)
    let freq = Random.float 200. +. 200. in
    (* Final frequency. *)
    let final = Random.int 6 + 14 in
    saw () freq
    >>= amp (1. /. float voices)
    >>= Stereo.pan ~law:`Circular () (Random.float 2. -. 1.)
  in
  let voices = List.init voices (fun _ -> voice ()) in
  Output.play (Stereo.mix voices >>= Stereo.amp 0.5)
