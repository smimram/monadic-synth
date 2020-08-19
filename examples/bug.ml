(** Recreating "Better off alone" by Alice DJ. *)

open Stream

let () =
  let tempo = 137. in
  let lead o =
    [
      0. , 0.5, `Note (71, 1.);
      1. , 0.5, `Note (71, 1.);
      1.5, 0.5, `Note (68, 1.);
      2.5, 0.5, `Note (71, 1.);
      3.5, 0.5, `Note (71, 1.);
      4.5, 0.5, `Note (70, 1.);
      5.5, 0.5, `Note (66, 1.);
      6. , 0.5, `Note (78+o, 1.);
      6.7, 0.5, `Note (78+o, 1.);
      7.3, 0.5, `Note (75, 1.);
      8. , 0. , `Nop
    ]
  in
  let lead = Pattern.append (lead 0) (lead (-2)) in
  let lead = Pattern.stream ~loop:true tempo lead in
  let lead = Instrument.play (Note.simple saw) lead in
  let drum =
    [
      0., `Kick 1.;
      0.5, `Snare 1.;
      1., `Nop;
    ]
  in
  let drum = Instrument.play_drums (Stream.timed ~loop:true ~tempo drum) >>= amp 2. in
  let bass =
    [
      0. , 4., `Note (40, 1.);
      4. , 4., `Note (39, 1.);
      8. , 4., `Note (44, 1.);
      12., 4., `Note (42, 1.);
    ]
  in
  (* let note = Note.adsr saw in *)
  let note () =
    let osc = square () in
    let lp = Filter.biquad () `Low_pass 3. in
    let ramp = Envelope.ramp ~kind:`Exponential () ~from:5000. ~target:100. 0.5 in
    fun freq -> bind2 lp ramp (osc freq)
  in
  let note = Note.adsr note in
  let bass = Instrument.play note (Pattern.stream ~loop:true tempo bass) in
  let bass = bass >>= Stream.Slicer.eurotrance () (60. /. tempo)  in
  let chords =
    [
      0. , 4., `Chord ([40;44;47;52], 1.);
      4. , 4., `Chord ([39;42;46;51], 1.);
      8. , 4., `Chord ([44;47;51;56], 1.);
      12., 4., `Chord ([42;46;49;54], 1.);
    ]
  in
  let arp = Pattern.arpeggiate `Up (Pattern.transpose 24 chords) in
  let arp = Instrument.play (Note.simple sine) (Pattern.stream ~loop:true tempo arp) in
  let pad = Instrument.play (Note.adsr sine) (Pattern.stream ~loop:true tempo (Pattern.transpose 12 chords)) in
  let s = B.mix [lead; drum; bass; arp; pad] >>= amp 0.2 in
  Output.play (s >>= stereo)
