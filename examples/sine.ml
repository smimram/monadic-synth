open Stream

let () =
  let s = sine () 440. >>= amp 0.2 in
  Output.play (s >>= stereo)
