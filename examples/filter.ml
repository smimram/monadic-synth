(** Testing filters. *)

open Stream

let () =
  let midi = MIDI.create () in
  (* let lpq = MIDI.controller midi 0 ~min:0.1 ~max:5. 1. in *)
  let lpf = MIDI.controller midi 0 ~min:10. ~max:40000. ~mode:`Logarithmic 2. >>= print "lpf" in
  let osc = saw () 1000. in
  let lp = bind1_2 (Filter.first_order ~variant:`Simple () `Low_pass) lpf in
  let s =
    osc
    >>= lp
    >>= stereo
  in
  Output.play s
