open Extlib
open Stream
open Stream.Common

type 'event note =
  {
    note : int;
    stream : sample stream;
    event : 'event Event.t;
    mutable released : bool;
    alive : bool ref;
  }

let create ~dt ~event ?(portamento=`None) (note:'a Note.t) =
  (* Currently playing notes. *)
  let playing = ref [] in
  let n = ref 0 in
  let stream : sample stream = fun () ->
    let x = List.fold_left (+.) 0. (List.map (fun n -> n.stream ()) !playing) in
    incr n;
    (* Regularly remove non-alive notes. *)
    if !n = 50000 then
      (
        n := 0;
        playing := List.filter (fun n -> !(n.alive)) !playing
      );
    x
  in
  (* let last_freq = ref None in *)
  let handler = function
    | `Note_on (n,v) ->
      let event = Event.create () in
      let alive = ref true in
      let on_die () = alive := false in
      let freq = Note.freq n in
      let stream = note ~dt ~event ~on_die freq v in
      (*
      let freq =
        match portamento with
        | `None -> return freq
        | `Linear p ->
          (
            match !last_freq with
            | None ->
              last_freq := Some freq;
              return freq
            | Some a ->
              let b = freq in
              last_freq := Some freq;
              ramp ~dt a b p
          )
      in
      let stream =
        let* freq = freq in
        note ~dt ~event ~on_die freq v
      in
     *)
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

let create_drum ~event note =
  let stream = ref blank in
  let on_die () = stream := blank in
  let handler = function
    | `Note_on (n,v) ->
      let freq = Note.freq n in
      stream := note ~on_die freq v
    | `Note_off _ -> ()
    | `Nop -> ()
  in
  Event.register event handler;
  let stream () = !stream () in
  stream

let emitter ~dt ?(loop=true) f l =
  let l0 = l in
  let l = ref l in
  let toff = ref 0. in
  let now = now ~dt () in
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

(** Play timed events. *)
let play ~dt (note:'a Note.t) events =
  let event = Event.create () in
  let s = create ~dt ~event note in
  emitter ~dt (Event.emit event) events >> s

(** Play a stream of lists events. *)
let play_stream ~dt (note:'a Note.t) =
  let event = Event.create () in
  let s = create ~dt ~event note in
  fun l ->
    List.iter (Event.emit event) l;
    s

let play_drum ~dt note events =
  let event = Event.create () in
  let s = create_drum ~event note in
  emitter ~dt (Event.emit event) events >> s

let play_drums ~dt ?kick ?snare ?closed_hat events =
  let streams = ref [] in
  let create d note =
    let dnote ~on_die freq vol = cmul vol (d ~dt ~on_die) in
    let note = Option.default dnote note in
    let event = Event.create () in
    let s = create_drum ~event note in
    streams := s :: !streams;
    event
  in
  let kick = create (fun ~dt ~on_die -> Note.Drum.kick ~dt ~on_die ()) kick in
  let snare = create (fun ~dt ~on_die -> Note.Drum.snare ~dt ~on_die ()) snare in
  let closed_hat = create (fun ~dt ~on_die -> Note.Drum.closed_hat ~dt ~on_die ()) closed_hat in
  let emit = function
    | `Kick v -> Event.emit kick (`Note_on (0,v))
    | `Snare v -> Event.emit snare (`Note_on (0,v))
    | `Closed_hat v -> Event.emit closed_hat (`Note_on (0,v))
    | `Nop -> ()
  in
  emitter ~dt emit events >> add_list !streams

let kick ~dt ?(vol=1.) tempo =
  play_drum ~dt (fun ~on_die freq vol -> cmul vol (Note.Drum.kick ~dt ~on_die ())) (Pattern.midi tempo [0.,1.,`Nop;0.,0.25,`Note(69,vol)])
