open Extlib
open Stream

let s ~dt =
  let buflen = 1 lsl 13 in
  let buf = Array.make buflen Complex.zero in
  let f0 = 500. in
  let kf = int_of_float (f0 *. float buflen *. dt) in
  buf.(kf) <- Complex.one;
  (* let buf = Array.init buflen (fun _ -> Complex.real (Random.float 1.)) in *)
  let buf = Array.map Complex.re (Sample.ifft buf) in
  let s = sampler ~dt ~freq:f0 buf 440. in
  s >>= amp 0.5 (* >>= Visu.graphics () *) >>= stereo

(* let s () = *)
  (* let s = cadd 100. (cmul 400. (now ~dt ())) >>= sine ~dt in *)
  (* (\* let s = sine ~dt 440. in *\) *)
  (* s >>= Visu.bands ~dt ~bands:4096 () >>= stereo *)

let s ~dt =
  (* let s = Spectral.harmonics ~dt () in *)
  let s = Spectral.pad ~dt () in
  s 440. >>= Visu.bands ~dt ~bands:4096 () >>= amp 0.08 >>= Stereo.schroeder ~dt

let () =
  Output.play s
