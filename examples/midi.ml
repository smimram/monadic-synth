open Stream

let s =
  let midi = MIDI.create () in
  let toggle n = MIDI.toggle midi n in
  let midi =
    let t = toggle 36 in
    MIDI.map midi
      (fun c e ->
         match e with
         | `Controller (n, v) ->
           let n = if get t then n + 8 else n in
           c, `Controller (n, v)
         | e -> c, e
      )
  in
  let knob n ?mode ?min ?max default = MIDI.controller midi ~channel:0 n ?mode ?min ?max default in
  let note =
    let a = knob 8 ~max:0.1 0.01 >>= print "a" in
    let d = knob 9 ~max:0.5 0.05 >>= print "d" in
    let s = knob 10 0.8 >>= print "s" in
    let r = knob 11 ~max:2. 0.1 >>= print "r" in
    Note.adsr ~a ~d ~s ~r saw
  in
  let note =
    let cents = knob 6 ~max:50. 7. in
    let wet = knob 7 0.5 in
    Note.detune ~cents ~wet note
  in
  let pad = Instrument.play (* ~portamento:(return 0.1) *) note (MIDI.events ~channel:0 midi) >>= clip in
  (* let pad = mul pad (knob 0 1.) in *)
  (* let pad = pad >>= amp 0.07 >>= Stereo.schroeder ~dt >>= Stereo.dephase ~dt (-0.01) in *)
  let pad =
    let lp = Filter.biquad () `Low_pass in
    let* q = knob 2 ~min:0.1 ~max:5. 1. >>= print "lpq"
    and* freq = knob 3  ~mode:`Logarithmic ~max:10000. 1500. >>= print "lpf"
    and* pad = pad in
    lp q freq pad
  in
  let pad = pad >>= amp 0.1 in
  (* let pad = bind2 (sample_and_hold ()) (Sample.every () 10) pad in *)
  let pad =
    let dephase = Stereo.dephase () in
    let smooth = smooth () 0.1 in
    let* delay =
      knob 4 0.01 ~min:(-0.1) ~max:0.1
      >>= smooth
      >>= initialize [-1.;1.]
      >>= print ~first:true "delay"
    in
    (* TODO: un commenting this makes the sound mono on right channel... *)
    (* let* delay = knob 67 ~max:0.1 0.01 in *)
    pad
    >>= stereo
    (* >>= Stereo.schroeder () *)
    >>= dephase delay
  in
  let pad = pad in
  (* let pad = exp_ramp () (-0.5) 1. 1. >>= Visu.graphics () >>= drop >> pad in *)
  let pad = blink_tempo (fun () -> MIDI.send midi 0 (`Note_on (4, 1.))) (fun () -> MIDI.send midi 0 (`Note_on (4, 0.))) 120. >> pad in
  pad

let () =
  (* OSC.server 8000; *)
  Output.play s
