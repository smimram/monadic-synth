open Msynth
open Stream

let s =
  let tempo = 138. in
  let pad = Pattern.concat [
    [0.,8.,`Chord([64;69;72],0.8);6.,1.,`Note(76,0.6);7.,1.,`Note(74,0.6)];
    [0.,8.,`Chord([64;68;71],0.8);6.,1.,`Note(74,0.6);7.,1.,`Note(72,0.6)];
    [0.,8.,`Chord([64;65;69],0.8);6.,1.,`Note(71,0.6);7.,1.,`Note(72,0.6)];
    [0.,8.,`Chord([64;68;71],0.8)];
  ]
  in
  let pad = Pattern.merge pad [0.,32.,`Note(40,2.5)] in
  let pad = Instrument.play (Note.simple sine) (Pattern.stream ~loop:true tempo pad) in
  let pad = pad >>= amp 0.07 >>= Stereo.schroeder () >>= Stereo.dephase () (-0.01) in
  let bass_note ~event ~on_die () =
    let adsr = adsr ~event ~on_die () ~a:0.01 ~d:0.1 ~r:0.001 () in
    let dup_adsr, adsr = dup () adsr in
    let fm = fm ~carrier:`Saw ~modulator:`Triangle () in
    (* let exp = Envelope.exponential_hl () in *)
    fun freq _ ->
      let s = dup_adsr >> B.cmul 500. adsr >>= (fun depth -> fm ~ratio:1. depth freq) in
      (* let r = exp (vol *. 0.04) in *)
      (* let r = cadd 500. (cmul (10000.*.vol**4.) r) in *)
      (* let s = bind2 (Filter.biquad ~dt `Low_pass 4.) r s in *)
      B.mul s adsr
  in
  let bass v = [0.,4.,`Nop; 0.,0.5,`Note (64,v); 0.75,0.5,`Note (64,v); 1.5,0.5,`Note (64,v); 2.5,0.5,`Note (64,v); 3.,0.5,`Note (65,v)] in
  let bass = Pattern.concat (List.map bass [0.6;0.7;0.8;1.]) in
  let bass = Pattern.transpose (-24) bass in
  let bass = Instrument.play bass_note (Pattern.stream ~loop:true tempo bass) in
  let bass = B.cmul 0.08 bass in
  (* let bass = bind3 (Filter.biquad ~dt `Low_pass) (OSC.float "/1/fader3" ~min:0.1 ~max:20. 0.5) (OSC.float "/1/fader4" ~min:1. ~max:5000. 5000.) bass in *)
  (* let bass = bass >>= stereo >>= Stereo.dephase ~dt 0.01 in *)
  let bass = bass >>= Stereo.schroeder2 () in
  let bass = let d = Note.duration tempo 0.5 in bass >>= Stereo.delay () d ~feedback:0.1 ~ping_pong:d in
  (* let bass2 = Instrument.play ~dt (Note.adsr ~a:0.01 ~d:0.05 ~r:0.1 (square ?width:None)) (Pattern.midi tempo bass2) in *)
  (* let bass2 = bass2 >>= amp 0.1 >>= Stereo.schroeder ~dt in *)
  (* let bass2 = let d = Note.duration tempo 0.5 in bass2 >>= Stereo.delay ~dt d ~feedback:0.1 ~ping_pong:d in *)
  (* let slice = [0.,8.,`Note(72,1.);8.,8.,`Note(71,1.)] in *)
  (* let slice = Instrument.play ~dt (Note.simple saw) (Pattern.midi tempo slice) in *)
  (* let slice = slice >>= Slicer.eurotrance ~dt (Note.duration tempo 1.) in *)
  (* let slice = slice >>= amp 0.4 >>= stereo in *)
  let pd = Instrument.play_drums ~snare:(fun ~on_die _ vol -> Note.Drum.snare ~on_die ~lp:2400. () >>= amp vol) in
  let drums = pd (Pattern.midi_drums ~loop:true tempo (Pattern.load_drums "basic.drums")) >>= amp 1. >>= stereo in
  (* let drums = *)
    (* let fv = Stereo.freeverb () in *)
    (* bind6 *)
      (* (fun roomsize damp width wet dry -> fv ~roomsize ~damp ~width ~dry ~wet) *)
      (* (OSC.float "/1/fader1" 0.5) *)
      (* (OSC.float "/1/fader2" ~min:1. ~max:0. 0.5) *)
      (* (OSC.float "/1/fader3" 1.) *)
      (* (OSC.float "/1/fader4" 0.3) *)
      (* (OSC.float "/1/fader5" 0.3) *)
      (* drums *)
  (* in *)
  let s = Stereo.mix [bass;drums;pad] >>= Stereo.amp 0.3 in
  s
  (* s >>= Visu.Stereo.bands ~dt ~amp:5. () *)

let () =
  OSC.server 8000;
  Output.play s
