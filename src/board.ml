(** A board stores all the parameters of a synthesizer and can display those. *)

open Stream

let create board =
  Graphics.open_graph "";
  let dx = 80 in
  let dy = 100 in
  let draw () =
    Graphics.set_color 0x000000;
    Graphics.fill_rect 0 0 (Graphics.size_x ()) (Graphics.size_y ());
    Graphics.set_color Graphics.cyan;
    List.iteri
      (fun j l ->
         List.iteri
           (fun i (label,c) ->
              let x = dx * i in
              let y = Graphics.size_y () - dy * j in
              (* Draw label *)
              let w,h = Graphics.text_size label in
              Graphics.moveto (x+dx/2-w/2) (y-dx-h);
              Graphics.draw_string label;
              (* Draw control *)
              match c with
              | `Knob(min,max,mode,s) ->
                Graphics.draw_circle (x+dx/2) (y-dx/2) 30;
                let v = get s in
                let v = Math.unstretch ~mode ~min ~max v in
                let a = int_of_float (225. -. v *. 270.) in
                Graphics.draw_arc (x+dx/2) (y-dx/2) 25 25 225 a;
              | `Switch s ->
                let r = 20 in
                (if get s then Graphics.fill_rect else Graphics.draw_rect) (x+dx/2-r/2) (y-dx/2-r/2) r r
           ) l
      ) board
  in
  draw ();
  let t = periodic ~on_reset:draw () 10. in
  t >>= drop
