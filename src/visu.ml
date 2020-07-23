(** Function for visualizing stream. *)

open Extlib
open Stream

let graphics () =
  Graphics.open_graph "";
  let x = ref 0 in
  let every = 441/2 in
  let e = ref (every-1) in
  let fg = Graphics.green in
  let bg = Graphics.black in
  let bar = Graphics.rgb 200 200 200 in
  Graphics.set_color bg;
  Graphics.fill_rect 0 0 (Graphics.size_x ()) (Graphics.size_y ());
  fun v ->
    incr e;
    if !e = every then
      (
        e := 0;
        let y = (v+.1.)/.2. in
        let sy = Graphics.size_y () in
        let y = int_of_float (y*.float sy) in
        Graphics.set_color bar;
        Graphics.moveto (!x+1) 0;
        Graphics.lineto (!x+1) sy;
        Graphics.set_color bg;
        Graphics.moveto !x 0;
        Graphics.lineto !x sy;
        Graphics.set_color fg;
        Graphics.plot !x y;
        incr x;
        if !x >= Graphics.size_x () then x := 0
      );
    return v

let bands ?(bands=1024) ?(scale=`Logarithmic) ?(amp=1.) ~dt =
  Graphics.open_graph "";
  let fg = Graphics.red in
  let buflen = 2 * bands in
  let buf = Array.make buflen Complex.zero in
  let bufpos = ref 0 in
  let every = int_of_float (0.5/.dt) in
  let e = ref (every-1) in
  let scale sx =
    match scale with
    | `Linear ->
      let w = sx / bands in
      (fun i -> i * w)
    | `Logarithmic ->
      let sx = float sx in
      let bands = float bands in
      (fun i -> int_of_float (log10 (float i/.bands*.9.+.1.) *. sx))
  in
  fun x ->
    buf.(!bufpos) <- Complex.real x;
    incr bufpos;
    if !bufpos = buflen then
      (
        bufpos := 0;
        e := !e + buflen;
        if !e >= every then
          (
            e := 0;
            let freq = Sample.fft (Spectral.Window.hamming buf) 0 (2*bands) in
            let scale = scale (Graphics.size_x ()) in
            let sy = float (Graphics.size_y ()) in
            Graphics.clear_graph ();
            Graphics.set_color fg;
            for i = 0 to bands - 1 do
              let h = int_of_float (Complex.norm freq.(i) *. amp *. sy) in
              Graphics.fill_rect (scale i) 0 (scale (i+1)) h
            done;
            let m = ref 0. in
            let mi = ref 0 in
            for i = 0 to buflen / 2 - 1 do
              let x = Complex.norm freq.(i) in
              if x > !m then (m := x; mi := i)
            done;
            Graphics.set_color Graphics.black;
            let t = Printf.sprintf "%f Hz (max: %f)" (float !mi /. float buflen /. dt) !m in
            let tx, ty = Graphics.text_size t in
            Graphics.moveto (Graphics.size_x () - tx) (Graphics.size_y () - ty);
            Graphics.draw_string t
          )
      );
    return x

module Stereo = struct
  let bands =
    let b = bands in
    fun ~dt ?bands ?amp () ->
      let bands = b ~dt ?bands ?amp in
      fun x -> Stereo.to_mono x >>= bands >>= drop >> return x
end
