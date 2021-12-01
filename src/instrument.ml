(** Instruments. *)

open Extlib
open Stream

(** A note of an instrument. *)
type ('sample, 'event) note =
  {
    note : int; (** Note: A4 is 69. *)
    stream : 'sample stream;
    event : 'event Event.t;
    mutable released : bool;
    alive : bool ref; (** is the note still playing? *)
  }

(** Create an instrument. *)
let create add ~event ?portamento (note:_ Note.t) =
  (* Currently playing notes. *)
  let playing = ref [] in
  let n = ref 0 in
  let stream =
    let* _ = dt in
    let ss = List.map (fun n -> n.stream) !playing in
    let* x = add ss in
    incr n;
    (* Regularly remove non-alive notes. *)
    if !n = 50000 then
      (
        n := 0;
        playing := List.filter (fun n -> !(n.alive)) !playing
      );
    return x
  in
  let last_freq = ref None in
  let handler = function
    | `Note_on (n,v) ->
      let event = Event.create () in
      let alive = ref true in
      let on_die () = alive := false in
      let freq = Note.frequency (float n) in
      (* let stream = note ~event ~on_die () freq v in *)
      let freq =
        match portamento with
        | None -> return freq
        | Some p ->
          (
            match !last_freq with
            | None ->
              last_freq := Some freq;
              return freq
            | Some a ->
              let b = freq in
              last_freq := Some freq;
              let ramp = Envelope.ramp () in
              (* let ramp = exp_ramp in *)
              let* p = p in
              ramp ~from:a ~target:b p
          )
      in
      let note = note ~event ~on_die () in
      let stream =
        let* freq = freq in
        note freq v
      in
      let note =
        {
          note = n;
          stream;
          event;
          released = false;
          alive;
        }
      in
      playing := note :: !playing
    | `Note_off n ->
      (
        try
          (* Only kill oldest alive note. *)
          List.rev_iter (fun note -> if note.note = n && not note.released && !(note.alive) then (note.released <- true; Event.emit note.event `Release; raise Exit)) !playing
        with
        | Exit -> ()
      )
    (* playing := List.filter (fun (n',_) -> n' <> n) !playing *)
    | _ -> ()
  in
  Event.register event handler;
  stream

let create_stereo ~event =
  let (+.) (x1,y1) (x2,y2) = x1 +. x2, y1 +. y2 in
  let add = StreamList.fold_left (+.) (0.,0.) in
  create add ~event

let create ~event =
  let add = StreamList.fold_left (+.) 0. in
  create add ~event

(** Create a drum instrument. *)
(* TODO: extend to polyphonic *)
let create_drum ~event note =
  let stream = ref blank in
  let on_die () = stream := blank in
  let handler = function
    | `Note_on (n,v) ->
      let freq = Note.frequency (float n) in
      stream := note ~on_die freq v
    | `Note_off _ -> ()
    | _ -> ()
  in
  Event.register event handler;
  let* dt = dt in
  return (!stream dt)

(*
let emitter ?(loop=true) f l =
  let l0 = l in
  let l = ref l in
  let toff = ref 0. in
  let now = now () in
  let rec aux time =
    match !l with
    | (t,e) :: tl when t +. !toff <= time ->
      f e;
      l := tl;
      aux time
    | [] ->
      if loop then
        (
          toff := time;
          l := l0;
          return ()
        )
      else
        return ()
    | _ -> return ()
  in
  now >>= aux
*)

(** Play a stream of lists events. *)
let play ?portamento (note:_ Note.t) midi =
  let event = Event.create () in
  let s = create ?portamento ~event note in
  midi >>= Event.emitter event >> s

let play_stereo ?portamento (note:_ Note.t) midi =
  let event = Event.create () in
  let s = create_stereo ?portamento ~event note in
  midi >>= Event.emitter event >> s

let play_drum note midi =
  let event = Event.create () in
  let s = create_drum ~event note in
  midi >>= Event.emitter event >> s

let play_drums ?kick ?snare ?closed_hat midi =
  let streams = ref [] in
  let create d note =
    let dnote ~on_die _ vol = B.cmul vol (d ~on_die) in
    let note = Option.value ~default:dnote note in
    let event = Event.create () in
    let s = create_drum ~event note in
    streams := s :: !streams;
    event
  in
  let kick = create (fun ~on_die -> Note.Drum.kick ~on_die ()) kick in
  let snare = create (fun ~on_die -> Note.Drum.snare ~on_die ()) snare in
  let closed_hat = create (fun ~on_die -> Note.Drum.closed_hat ~on_die ()) closed_hat in
  let emit = function
    | `Kick v -> Event.emit kick (`Note_on (0,v))
    | `Snare v -> Event.emit snare (`Note_on (0,v))
    | `Closed_hat v -> Event.emit closed_hat (`Note_on (0,v))
    | `Nop -> ()
  in
  midi >>= (fun l -> return (List.iter emit l)) >> B.mix !streams

let kick tempo =
  let event = Event.create () in
  let note ~on_die _ _ = Note.Drum.kick ~on_die () in
  let instr = create_drum ~event note in
  let midi = Pattern.stream ~loop:true tempo [0.,1.,`Nop;0.,0.25,`Note(69,1.)] in
  let* l = midi in
  List.iter (Event.emit event) l;
  instr
