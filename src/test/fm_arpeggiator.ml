open Stream

let tempo = 130.

(** Nice FM arpegiator. *)
let s ~dt =
  let s = Pattern.concat [[0.,8.,`Chord([72;76;79],1.)];[0.,8.,`Chord([71;72;76;79],1.)];[0.,8.,`Chord([69;72;76;79],1.)];[0.,8.,`Chord([67;71;76;79],1.)]] in
  let s = Pattern.transpose (-12) s in
  let s = Pattern.arpeggiate tempo ~note:0.25 `Up_down s in
  (* let d = OSC.float "/1/fader1" ~mode:`Logarithmic ~max:10000. 100. in *)
  let d = cmul 100. (now ~dt ()) in
  let note ~dt ~event ~on_die freq vol =
    let adsr = adsr ~dt ~event ~on_die () ~a:0.01 ~d:0.1 ~r:0.001 in
    let dup_adsr, adsr = dup () adsr in
    let fm = fm ~dt ~carrier:`Saw ~modulator:`Triangle () in
    let s = dup_adsr >> mul d adsr >>= (fun depth -> fm ~ratio:0.5 depth freq) in
    mul s adsr
  in
  let s = Instrument.play ~dt note (Pattern.midi tempo s) >>= amp 0.1 in
  let s = bind3 (Filter.biquad ~dt `Low_pass) (OSC.float "/1/fader3" ~min:0.1 ~max:20. 0.5) (OSC.float "/1/fader4" ~max:10000. 10000.) s in
  (* let s = s >>= agc ~dt () in *)
  let s = s >>= stereo  >>= Stereo.dephase ~dt (-0.01) in
  (* let kick = Instrument.kick ~dt ~vol:1. tempo >>= amp 0.7 >>= stereo in *)
  (* Stereo.add s kick *)
  s

let () =
  OSC.server 8000;
  Output.play s
