open Stream

let s =
  let tempo = 138. in
  let bass_note ~event ~on_die () =
    let adsr = adsr ~event ~on_die () ~a:0.01 ~d:0.1 ~r:0.001 () in
    let saw = saw () in
    let fm = fm ~carrier:`Saw ~modulator:`Triangle () in
    fun freq vol ->
      let* x = adsr in
      cmul x (* (fm ~ratio:1. (500. *. x) freq) *) (saw freq)
  in
  let bass_note = Note.adsr saw in
  let bass v = [0.,4.,`Nop; 0.,0.5,`Note (64,v); 0.75,0.5,`Note (64,v); 1.5,0.5,`Note (64,v); 2.5,0.5,`Note (64,v); 3.,0.5,`Note (65,v)] in
  let bass = Pattern.concat (List.map bass [0.6;0.7;0.8;1.]) in
  let bass = Pattern.transpose (-24) bass in
  let bass = Instrument.play bass_note (Pattern.midi tempo bass) in
  let bass = cmul 0.08 bass in
  (* let bass = bass >>= Stereo.schroeder2 () in *)
  let bass = bass >>= stereo in
  (* let bass = let d = Note.duration tempo 0.5 in bass >>= Stereo.delay () d ~feedback:0.1 ~ping_pong:d in *)
  let s = bass >>= Stereo.amp 0.3 in
  s
(* s >>= Visu.Stereo.bands ~dt ~amp:5. () *)

let s =
  let a = adsr () ~a:1. ~d:0.5 ~s:0.5 () in
  mul a (sine () 440.) >>= stereo

let () =
  OSC.server 8000;
  Output.play s
