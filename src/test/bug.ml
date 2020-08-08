open Stream

let s =
  let midi = MIDI.create () in
  let note = karplus_strong in
  (* let note = sine in *)
  let note = Note.adsr note in
  let synth = Instrument.play_stream note in
  MIDI.events ~channel:0 midi >>= synth >>= amp 0.2 >>= stereo

let () =
  Output.play s
