open Extlib
module Note = Stream.Note

(** Musical patterns. Events in patterns are (time,duration,event). All time
    units are bpm here. *)
type 'event t = (float * float * 'event) list

(* TODO: handle unsorted patterns? *)
let duration (p:'a t) =
  List.fold_left (fun m (t,d,_) -> max m (t +. d)) 0. p

let offset o (p:'a t) : 'a t = List.map (fun (t,d,e) -> t+.o,d,e) p

let merge p1 p2 : 'a t = p1@p2

let append p1 p2 =
  merge p1 (offset (duration p1) p2)

let concat pp = List.fold_left append [] pp

let repeat n p : 'a t =
  let d = duration p in
  let rec aux k =
    if k = n then [] else
      (offset (d *. float k) p)@(aux (k+1))
  in
  aux 0

let transpose t (p : 'a t) : 'a t =
  let f n = n + t in
  List.map
    (fun (t,d,e) ->
      t,d,
      match e with
      | `Chord (l,v) -> `Chord (List.map f l,v)
      | `Note (n,v) -> `Note (f n,v)
      | `Nop -> `Nop)
    p

let amplify a (p : 'a t) : 'a t =
  List.map
    (fun (t,d,e) ->
      t,d,
      match e with
      | `Chord (l,v) -> `Chord(l,a*.v)
      | `Note (n,v) -> `Note (n,a*.v)
      | `Nop -> `Nop)
    p

let arpeggiate tempo ?(note=0.25) mode (p : 'a t) : 'a t =
  let ans =
    List.map
      (fun (t,d,e) ->
        match e with
        | `Chord (l,v) ->
          let ans = ref [] in
          let add t' d n = ans := (t+.t',d,`Note (n,v)) :: !ans in
          let notes = int_of_float (d /. note) in
          (
            match mode with
            | `Up_down ->
              let n = match l with
                | [n1;n2;n3] -> [|n1;n2;n3;n1+12;n2+12;n1+12;n3;n2|]
                | [n1;n2;n3;n4] -> [|n1;n2;n3;n4;n1+12;n4;n3;n2|]
                | _ -> assert false
              in
              for i = 0 to notes - 1 do
                add (float i *. note) note n.(i mod 8)
              done
            | `Staccato ->
              for i = 0 to notes - 1 do
                List.iter (fun n -> add (float i *. note) note n) l
              done
          );
          !ans
        | e -> [t,d,e]
      ) p
  in
  List.flatten ans

(** Convert a pattern to timed MIDI events. *)
let midi bpm (p:'a t) =
  let duration = Note.duration bpm in
  let ans = ref [] in
  let emit t e = ans := (t,e) :: !ans in
  let rec aux = function
    | t,d,`Note (n,v) ->
      emit (duration t) (`Note_on (n,v));
      emit (duration t +. duration d) (`Note_off n)
    | t,d,`Chord (l,v) ->
      let l = List.map (fun n -> t,d,`Note (n,v)) l in
      List.iter aux l
    | t,d,`Nop -> emit (duration t +. duration d) `Nop
  in
  List.iter aux p;
  List.sort (fun (t1,_) (t2,_) -> compare t1 t2) !ans

let midi_drums bpm (p:'a t) =
  let duration = Note.duration bpm in
  let ans = ref [] in
  let emit t e = ans := (t,e) :: !ans in
  let aux (t,d,e) =
    let t = if e = `Nop then duration (t+.d) else duration t in
    emit t e
  in
  List.iter aux p;
  List.sort (fun (t1,_) (t2,_) -> compare t1 t2) !ans

let load_drums fname : 'a t =
  let lines = Str.split (Str.regexp "\n") (File.to_string fname) in
  let ans = ref [] in
  let add t d e = ans := (t,d,e) :: !ans in
  let line_re = Str.regexp "^\\([a-zA-Z]+\\)(\\([0-9\\.]*\\))[ ]*:\\([X ]*\\)$" in
  (* TODO: parameter for duration of characters *)
  let duration = 1. /. 4. in
  List.iter (fun l ->
    assert (Str.string_match line_re l 0);
    let name = Str.matched_group 1 l in
    let vol = Str.matched_group 2 l in
    let vol = if vol = "" then 1. else float_of_string vol in
    let e =
      match name with
      | "KD" | "BD" -> `Kick vol
      | "SD" -> `Snare vol
      | "CH" -> `Closed_hat vol
      | _  -> `Nop
    in
    let pattern = Str.matched_group 3 l in
    let len = String.length pattern in
    add 0. (float len *. duration) `Nop;
    for i = 0 to len - 1 do
      if pattern.[i] = 'X' then
        add (float i *. duration) duration e
    done;
  ) lines;
  !ans
