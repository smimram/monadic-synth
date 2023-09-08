open Msynth
open Stream

let s = Stream.sine () 440. >>= Stereo.of_mono

let () = Output.play s
