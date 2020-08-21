(* Trying to recreate Oberheim OB-Xa. *)

open Extlib
open Stream

let synth
    ?(master_volume=cst 1.)
    ?(detune=cst 0.1) (* detuning in semitone *)
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
    ?(osc2_detune=cst 1.01)
    ?(sub_volume=cst 0.5)
    ?(noise_volume=cst 0.5)
    ?(a=cst 0.01) ?(d=cst 0.05) ?(s=cst 0.8) ?(r=cst 0.1)
    ?(lp_q=cst 1.)
    ?(lp_f=cst 5000.)
    ?(lp_a=cst 0.1)
    ?(lp_d=cst 10.)
    ?(lp_s=cst 0.1)
    ?(lp_r=cst 0.1)
    ?(lp_4pole=cst true)
    ?(portamento=cst 0.)
    ?(pitch_bend=cst 0.)
    e
  =
  let lfo = bind2 (osc ()) lfo_form lfo_rate in
  let lfo_, lfo = dup () lfo in
  let note : _ Note.t =
    fun ~event ~on_die () ->
    let tuning () =
      let d = get detune /. 12. in
      1. +. Random.float ~min:(-.d) d
    in
    let unison = get unison in
    let stereo_amount = get stereo_amount in
    let stereo_mode = if stereo_amount = 0. then `Mono else get stereo_mode in
    let stereo_coeff () = let d = 0.1 *. stereo_amount in 1. +. Random.float ~min:(-.d) d in
    let stereo_coeff_l = stereo_coeff () in
    let stereo_coeff_r = stereo_coeff () in
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
        let  o2 = osc2 ~width:((1. +. lfo *. lfo_pwm2) /. 2.) s2 (freq *. detune2) in
        let* v2 = osc2_volume in
        let* x2 = scmul v2 o2 in
        return (x1 +. x2)
    in
    let osc =
      List.init
        (if stereo_mode = `Spread then 2 * unison else unison)
        (fun i ->
           osc (),
           tuning (),
           match stereo_mode with
           | `Mono -> 0.
           | `Spread -> if i < unison then -.stereo_amount else stereo_amount
           | `Pan -> Random.float ~min:(-.stereo_amount) stereo_amount
        )
    in
    let lp_adsr = adsr ~event () ~a:(get lp_a) ~d:(get lp_d) ~s:(get lp_s) ~r:(get lp_r) in
    let adsr = adsr ~event ~on_die () ~a:(get a) ~d:(get d) ~s:(get s) ~r:(get r) in
    let lpl = lp_4pole >>= switch (Filter.ladder () `Low_pass) (Filter.biquad () `Low_pass) in
    let lpr = lp_4pole >>= switch (Filter.ladder () `Low_pass) (Filter.biquad () `Low_pass) in
    (* Noise *)
    let noise = Stream.osc () `Noise 0. >>= smulc noise_volume >>= stereo in
    (* Sub-oscillator *)
    let sub =
      let osc = Stream.osc () `Saw in
      fun freq -> osc (freq /. 2.) >>= smulc sub_volume >>= stereo
    in
    fun freq vol ->
      (* Pitch bend *)
      let* pitch_bend = pitch_bend in
      let freq = freq *. (2. ** (pitch_bend /. 12.)) in
      (* Low pass filter *)
      let* lp_q = lp_q in
      let* lp_f = lp_f in
      let* lp_adsr = lp_adsr () in
      let lpl = lpl lp_q (lp_f *. lp_adsr *. stereo_coeff_l) in
      let lpr = lpr lp_q (lp_f *. lp_adsr *. stereo_coeff_r) in
      let l = List.map (fun (osc,d,p) -> osc (freq *. d) >>= Stereo.pan p) osc in
      let l = noise::(sub freq)::l in
      let* a = adsr () in
      Stereo.mix l >>= Stereo.map lpl lpr >>= Stereo.amp (a *. vol)
  in
  (* let reverb = Stereo.converb ~duration:0.01 () in *)
  let s = Instrument.play_stereo ~portamento note e in
  let* unison = unison in
  let* vol = master_volume in
  lfo_
  >> s
  >>= Stereo.amp (0.1 *. vol /. float unison)
  (* >>= reverb *)

let () =
  let midi = MIDI.create () in
  let shift = MIDI.toggle midi 36 in
  let midi =
    let t = shift in
    MIDI.map midi
      (fun c e ->
         match e with
         | `Controller (n, v) ->
           let n = if get t then n + 8 else n in
           c, `Controller (n, v)
         | e -> c, e
      )
  in
  let midi = MIDI.print midi in
  let knob n ?mode ?min ?max default = MIDI.controller midi n ?mode ?min ?max default in
  let detune = knob 0 ~max:0.25 0.01 in
  let stereo_amount = knob 4 0.5 >>= print "sa" in
  let osc2_volume = knob 1 1. in
  let lfo_rate = knob 5 ~max:10. 2. >>= print "lfo rate" in
  let lfo_pwm1 = knob 2 0.5 in
  let lfo_pwm2 = knob 6 0.5 in
  let lp_f = knob 3 ~mode:`Logarithmic ~min:10. ~max:20000. 10000. >>= print "lp f" in
  let lp_q = knob 7 ~min:0.1 ~max:5. 1. >>= print "lp q" in
  let a = knob 8 0.01 >>= print "a" in
  let d = knob 9 0.01 >>= print "d" in
  let s = knob 10 0.8 >>= print "s" in
  let r = knob 11 ~max:4. 0.1 >>= print "r" in
  let sustain = s in
  let lp_a = knob 12 0.01 >>= print "lpa" in
  let lp_d = knob 13 0.01 >>= print "lpd" in
  let lp_s = knob 14 0.8 >>= print "lps" in
  let lp_r = knob 15 ~max:4. 0.1 >>= print "lpr" in
  let pitch_bend = MIDI.pitch_bend midi () in
  let s = synth ~detune ~stereo_amount ~osc2_volume ~lfo_rate ~lfo_pwm1 ~lfo_pwm2 ~lp_f ~lp_q ~a ~d ~s ~r ~lp_a ~lp_d ~lp_s ~lp_r ~pitch_bend (MIDI.events midi) in
  (* Board. *)
  let board =
    Board.create
      [
        [
          "detune",`Knob(0.,0.25,`Linear,detune);
          "osc2 vol",`Knob(0.,1.,`Linear,osc2_volume);
          "lfp pwm1",`Knob(0.,1.,`Linear,lfo_pwm1);
          "lp freq",`Knob(10.,20000.,`Logarithmic,lp_f);
        ];
        [
          "stereo",`Knob(0.,1.,`Linear,stereo_amount);
          "lfo rate",`Knob(0.,10.,`Linear,lfo_rate);
          "lfp pwm2",`Knob(0.,1.,`Linear,lfo_pwm2);
          "lp q",`Knob(0.1,5.,`Logarithmic,lp_q);
        ];
        [
          "a",`Knob(0.,1.,`Linear,a);
          "d",`Knob(0.,1.,`Linear,d);
          "s",`Knob(0.,1.,`Linear,sustain);
          "r",`Knob(0.,4.,`Linear,r);
          "shift",`Switch shift;
        ];
        [
          "lp a",`Knob(0.,1.,`Linear,lp_a);
          "lp d",`Knob(0.,1.,`Linear,lp_d);
          "lp s",`Knob(0.,1.,`Linear,lp_s);
          "lp r",`Knob(0.,4.,`Linear,lp_r);
        ]
      ]
  in
  let s = board >> s in
  (* LED animation *)
  let _ = Thread.create
      (fun () ->
         for n = 0 to 7 do
           let n = if n < 4 then 13+n else 5+n in
           Unix.sleepf 0.04;
           MIDI.send midi 0 (`Note_on (n, 1.));
         done;
         for n = 0 to 7 do
           let n = if n < 4 then 13+n else 5+n in
           Unix.sleepf 0.04;
           MIDI.send midi 0 (`Note_on (n, 0.));
         done) ()
  in
  Output.play s
