open Stream

let tempo = 130.

let s =
  let synth = Pattern.concat [[0.,8.,`Chord([69;71;72;76],1.)];[0.,8.,`Chord([68;71;74;76],1.)]] in
  let synth = Pattern.arpeggiate tempo `Up synth in
  let synth = Pattern.transpose (-12) synth in

  let sound () = saw () in
  let synth = Instrument.play (Note.adsr ~r:(return 0.01) sound) (Pattern.stream ~loop:true tempo synth) in
  let lp = Filter.biquad () `Low_pass in
  let synth =
    let lp_freq = OSC.float ~mode:`Logarithmic "/oscControl/slider1" ~max:10000. 1500. in
    let lp_q = OSC.float "/oscControl/slider2" ~min:0.1 ~max:5. 1. in
    bind3 lp lp_q lp_freq synth
  in
  let synth = synth >>= stereo in
  let synth = synth >>= Stereo.dephase () 0.01 in
  let s = Stereo.add_list [synth] in
  Stereo.cmul 0.6 s

let () =
  OSC.server 10000;
  Output.play s
