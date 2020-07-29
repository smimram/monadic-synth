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
  let note = Note.adsr saw in
  let midi_dup, midi = Stream.dup () (Stream.Events.midi ()) in
  let pad = midi >>= Stream.Events.all_channels >>= Instrument.play_stream ~dt note >>= clip in
  let pad = mul pad (midi >>= Stream.Events.controller 1 1.) in
  (* let pad = pad >>= amp 0.07 >>= Stereo.schroeder ~dt >>= Stereo.dephase ~dt (-0.01) in *)
  let pad =
    let lp = Filter.biquad ~dt `Low_pass in
    let q = midi >>= Stream.Events.controller 3 ~min:0.1 ~max:5. 1. in
    let freq = midi >>= Stream.Events.controller 4  ~mode:`Logarithmic ~max:10000. 1500. in
    bind3 lp q freq pad
  in
  midi_dup >> pad >>= Stereo.of_mono

let () =
  (* OSC.server 8000; *)
  Output.play s
