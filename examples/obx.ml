(* Trying to recreate Oberheim OB-Xa. *)

open Extlib
open Stream

let synth
    ?(master_volume=cst 1.)
    ?(detune=0.01) (* detuning in semitone *)
    ?(unison=cst 1) (* number of unison channels *)
    ?(stereo_amount=cst 0.5)
    ?(stereo_mode=cst `Spread)
    ?(lfo_form=cst `Sine)
    ?(lfo_rate=cst 2.)
    ?(lfo_pwm1=cst 0.5)
    ?(lfo_pwm2=cst 0.5)
    ?(osc1_shape=cst `Saw)
    ?(osc2_shape=cst `Square)
    ?(osc2_volume=cst 1.)
    ?(osc2_detune=cst (1.01/.2.))
    ?a ?d ?s ?r
    ?(lp_q=cst 1.)
    ?(lp_f=cst 5000.)
    ?(lp_a=cst 0.1)
    ?(lp_d=cst 10.)
    ?(lp_s=cst 0.1)
    ?(lp_r=cst 0.1)
    e
  =
  let lfo = bind2 (osc ()) lfo_form lfo_rate in
  let lfo_, lfo = dup () lfo in
  let note : _ Note.t =
    fun ~event ~on_die () ->
    let tuning () =
      let d = detune /. 12. in
      1. +. Random.float ~min:(-.d) d
    in
    let unison = get unison in
    let stereo_amount = get stereo_amount in
    let stereo_mode = get stereo_mode in
    let osc () =
      let osc1 = osc () in
      let osc2 = osc () in
      fun freq ->
        let* s1 = osc1_shape in
        let* s2 = osc2_shape in
        let* lfo = lfo in
        let* lfo_pwm1 = lfo_pwm1 in
        let* lfo_pwm2 = lfo_pwm2 in
        let* detune2 = osc2_detune in
        let* x1 = osc1 ~width:((1. +. lfo *. lfo_pwm1) /. 2.) s1 freq in
        let* x2 = osc2 ~width:((1. +. lfo *. lfo_pwm2) /. 2.) s2 (freq *. detune2) in
        let* v2 = osc2_volume in
        return (x1 +. v2 *. x2)
    in
    let osc =
      List.init
        (if stereo_mode = `Spread then 2 * unison else unison)
        (fun i ->
           osc (),
           tuning (),
           match stereo_mode with
           | `None -> 0.
           | `Spread -> if i < unison then -.stereo_amount else stereo_amount
           | `Pan -> Random.float ~min:(-.stereo_amount) stereo_amount
        )
    in
    let lp_adsr = adsr ~event () ~a:(get lp_a) ~d:(get lp_d) ~s:(get lp_s) ~r:(get lp_r) in
    let adsr = adsr ~event ~on_die () ?a ?d ?s ?r in
    let lpl = Filter.biquad () `Low_pass in
    let lpr = Filter.biquad () `Low_pass in
    fun freq vol ->
      let* lp_q = lp_q in
      let* lp_f = lp_f in
      let* lp_adsr = lp_adsr () in
      let lpl = lpl lp_q (lp_f *. lp_adsr) in
      let lpr = lpr lp_q (lp_f *. lp_adsr) in
      let l = List.map (fun (osc,d,p) -> osc (freq *. d) >>= Stereo.pan () p) osc in
      let* a = adsr () in
      Stereo.mix l >>= Stereo.map lpl lpr >>= Stereo.amp (a *. vol)
  in
  let s = Instrument.play_stereo note e in
  let* unison = unison in
  let* vol = master_volume in
  lfo_
  >> s
  >>= Stereo.amp (0.1 *. vol /. float unison)

let () =
  let midi = MIDI.create () in
  let s = synth (MIDI.events midi) in
  Output.play s
