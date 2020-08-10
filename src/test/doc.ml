open Stream

let () =
  let s =
    let* x = sine () 440. in
    stereo x
  in
  Output.play s

let () =
  let s = sine () 440. >>= stereo in
  Output.play s
