open Stream

let s ~dt =
  (* let tempo = 138. in *)
  (* let pad = Pattern.concat [ *)
    (* [0.,8.,`Chord([64;69;72],0.8);6.,1.,`Note(76,0.6);7.,1.,`Note(74,0.6)]; *)
    (* [0.,8.,`Chord([64;68;71],0.8);6.,1.,`Note(74,0.6);7.,1.,`Note(72,0.6)]; *)
    (* [0.,8.,`Chord([64;65;69],0.8);6.,1.,`Note(71,0.6);7.,1.,`Note(72,0.6)]; *)
    (* [0.,8.,`Chord([64;68;71],0.8)]; *)
  (* ] *)
  (* in *)
  (* let pad = Pattern.merge pad [0.,32.,`Note(40,2.5)] in *)
  (* let pad = sine ~dt 440. in *)
  let midi = MIDI.create () in
  let knob n ?mode ?min ?max default = MIDI.controller midi ~channel:0 n ?mode ?min ?max default in
  let note =
    let a = knob 4 ~max:0.1 0.01 >>= print "a" in
    let d = knob 5 ~max:0.5 0.05 >>= print "d" in
    let s = knob 6 0.8 >>= print "s" in
    let r = knob 7 ~max:2. 0.1 >>= print "r" in
    Note.adsr ~a ~d ~s ~r saw
  in
  let note = Note.detune ~cents:(knob 0 ~max:50. 7.) ~wet:(knob 1 0.5) note in
  let pad = MIDI.events ~channel:0 midi >>= Instrument.play_stream ~dt note >>= clip in
  (* let pad = mul pad (knob 0 1.) in *)
  (* let pad = pad >>= amp 0.07 >>= Stereo.schroeder ~dt >>= Stereo.dephase ~dt (-0.01) in *)
  let pad =
    let lp = Filter.biquad ~dt `Low_pass in
    let* q = knob 2 ~min:0.1 ~max:5. 1. >>= print "lpq"
    and+ freq = knob 3  ~mode:`Logarithmic ~max:10000. 1500. >>= print "lpf"
    and+ pad = pad in
    lp q freq pad
  in
  let pad = pad >>= amp 0.1 >>= Stereo.of_mono >>= Stereo.dephase ~dt 0.01 in
  (* adsr ~a:0.01 ~d:0.5 ~s:0. ~r:2. ~dt () >>= Visu.graphics () >>= drop >> pad *)
  pad

let () =
  (* OSC.server 8000; *)
  Output.play s
