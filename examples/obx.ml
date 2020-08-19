(* Trying to recreate Oberheim OB-Xa. *)

open Extlib
open Stream

let synth
    ?(master_volume=cst 1.)
    ?(detune=0.05) (* detuning in semitone *)
    ?(unison=cst 2) (* number of unison channels *)
    ?(stereo_amount=cst 0.5)
    ?(stereo_mode=cst `Spread)
    e
  =
  let note : _ Note.t =
    fun ~event ~on_die () ->
    let tuning () =
      let d = detune /. 12. in
      1. +. Random.float ~min:(-.d) d
    in
    let unison = get unison in
    let stereo_amount = get stereo_amount in
    let stereo_mode = get stereo_mode in
    let osc =
      List.init
        (if stereo_mode = `Spread then 2 * unison else unison)
        (fun i ->
           saw (),
           tuning (),
           match stereo_mode with
           | `None -> 0.
           | `Spread -> if i < unison then -.stereo_amount else stereo_amount
           | `Pan -> Random.float ~min:(-.stereo_amount) stereo_amount
        )
    in
    let adsr = adsr ~event ~on_die () in
    fun freq vol ->
      let l = List.map (fun (osc,d,p) -> osc (freq *. d) >>= Stereo.pan () p) osc in
      let* a = adsr () in
      Stereo.mix l >>= Stereo.amp (a *. vol)
  in
  let s = Instrument.play_stereo note e in
  let* vol = master_volume in
  s >>= Stereo.amp (0.1 *. vol)

let () =
  let midi = MIDI.create () in
  let s = synth (MIDI.events midi) in
  Output.play s
