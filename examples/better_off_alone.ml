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
  let lead = Instrument.play (Note.simple saw) (Pattern.stream ~loop:true tempo lead) in
  let drum =
    [
      0., `Kick 1.;
      0.5, `Snare 1.;
      1., `Nop;
    ]
  in
  let drum = Instrument.play_drums (Stream.timed ~loop:true ~tempo drum) >>= amp 2. in
  let s = mix [lead;drum] >>= amp 0.2 in
  Output.play (s >>= Stereo.schroeder ())
