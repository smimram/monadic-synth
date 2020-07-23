open Stream

let tempo = 130.

let s ~dt =
  let synth = Pattern.concat [[0.,8.,`Chord([72;76;79],1.)];[0.,8.,`Chord([71;72;76;79],1.)];[0.,8.,`Chord([69;72;76;79],1.)];[0.,8.,`Chord([67;71;76;79],1.)]] in
  (* let synth = Pattern.merge synth (Pattern.transpose 24 (Pattern.amplify 1.5 (Pattern.arpeggiate tempo ~note:(1./.8.) `Up_down synth))) in *)
  (* let synth = Pattern.arpeggiate tempo ~note:(1./.8.) `Up_down synth in *)
  let synth = Pattern.transpose (-12) synth in
  let sound ~dt freq = cmul 0.5 (add (square ~dt (freq *. 1.007)) (saw ~dt freq)) in
  let synth = Instrument.play ~dt (Note.simple sound) (Pattern.midi tempo synth) in
  let lp_q = OSC.float "/1/fader1" ~min:0.1 ~max:5. 1. in
  let lp_freq = OSC.float ~mode:`Logarithmic "/1/fader2" ~max:10000. 1500. in
  (* let lp_freq = lp_freq >>= print ~every:22000 "freq" in *)
  let slicer = Slicer.staccato ~dt () ~s:0.5 in
  let slicer lp_q lp_freq = slicer ~lp_q ~lp_freq (Note.duration tempo 0.5) in
  let synth = bind3 slicer lp_q lp_freq synth in
  let synth = cmul 0.2 synth in
  (* let synth = *)
    (* let flanger = flanger ~dt 0.01 in *)
    (* bind2 (fun wet -> flanger ~wet (1. /. Note.duration tempo 4.)) (OSC.float "/1/fader2" ~max:0.8 0.1) synth *)
  (* in *)
  (* let synth = bind3 (Filter.biquad ~dt `High_pass) (OSC.float "/1/fader3" ~min:0.01 ~max:5. 1.) (OSC.float ~mode:`Logarithmic "/1/fader4" ~max:10000. 1500.) synth in *)
  (* let synth = synth >>= Distortion.convolver ~dt 0.4 in *)
  let synth = synth >>= stereo in
  (* let synth = let d = Note.duration tempo 0.5 in synth >>= Stereo.delay ~dt d ~feedback:0.1 ~ping_pong:d in *)
  let synth = synth >>= Stereo.dephase ~dt 0.01 in
  let synth = Stereo.bmul (OSC.bool "/1/toggle1" true) synth in
  let bass = [72;71;69;67] in
  let bass = List.map (fun n -> Pattern.repeat 4 [0.,2.,`Nop; 0.,0.5,`Note (n,1.); 0.5,0.5,`Note (n,1.)]) bass in
  let bass = Pattern.concat bass in
  let bass = Pattern.transpose (-24) bass in
  let bass = Instrument.play ~dt (Note.adsr ~r:0.1 sine) (Pattern.midi tempo bass) in
  let bass = cmul 0.5 bass in
  let bass = bass >>= stereo >>= Stereo.dephase ~dt (-0.02) in
  (* let kick = Instrument.kick ~dt ~vol:1. tempo >>= stereo in *)
  let pd = Instrument.play_drums ~dt ~snare:(fun ~on_die freq vol -> cmul vol (Note.Drum.snare ~dt ~on_die ~lp:2000. ())) in
  let drums = pd (Pattern.midi_drums tempo (Pattern.load_drums "c1.drums")) >>= stereo in
  let drums = Stereo.bmul (OSC.bool "/1/toggle2" true) drums in
  let s = Stereo.add_list [synth;drums;bass] in
  (* let s = s >>= Stereo.map (agc ~dt ()) (agc ~dt ()) in *)
  Stereo.cmul 0.6 s

let () =
  OSC.server 8000;
  Output.play s
