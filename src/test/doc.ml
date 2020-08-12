open Stream

let pair x y = return (x, y)

(* let () = *)
  (* let s = *)
    (* let* x = sine () 440. in *)
    (* let* y = sine () 880. in *)
    (* pair x y *)
  (* in *)
  (* Output.play s *)

(* let () = *)
  (* let left = sine () in *)
  (* let right = sine () in *)
  (* let s = *)
    (* let* x = left 440. in *)
    (* let* y = right 880. in *)
    (* pair x y *)
  (* in *)
  (* Output.play s *)

let () =
  let lfo = sine () in
  let vco = sine () in
  let s =
    let* f = lfo 20. in
    vco (440. +. f)
  in
  Output.play (s >>= stereo)

(*
let () =
  let s = bind stereo (sine () 440.) in
  Output.play s

let () =
  let s =
    let* x = sine () 440. in
    stereo x
  in
  Output.play s

let () =
  let s = sine () 440. >>= stereo in
  Output.play s
*)
