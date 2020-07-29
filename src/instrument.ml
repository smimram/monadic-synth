open Extlib
open Stream
open Stream.Common

type 'event note =
  {
    note : int;
    stream : sample stream;
    event : 'event Event.t;
    alive : bool ref;
  }

let create ~dt ~event (note:'a Note.t) =
  let playing = ref [] in
  let n = ref 0 in
  let stream : sample stream = fun () ->
    let x = List.fold_left (+.) 0. (List.map (fun n -> n.stream ()) !playing) in
    incr n;
    if !n = 10000 then
      (
        n := 0;
        playing := List.filter (fun n -> !(n.alive)) !playing
      );
    x
  in
  let handler = function
    | `Note_on (n,v) ->
      let freq = Note.freq n in
      let event = Event.create () in
      let alive = ref true in
      let on_die () = alive := false in
      let stream = note ~dt ~event ~on_die freq v in
      let note =
        {
          note = n;
          stream;
          event;
          alive;
        }
      in
      playing := note :: !playing
    | `Note_off n ->
      (
        try
          (* Only kill oldest alive note. *)
          List.rev_iter (fun note -> if note.note = n && !(note.alive) then (Event.emit note.event `Release; raise Exit)) !playing
        with
        | Exit -> ()
      )
    (* playing := List.filter (fun (n',_) -> n' <> n) !playing *)
    | `Nop -> ()
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

(** Play a stream of lists events. *)
let play_stream ~dt (note:'a Note.t) events =
  let event = Event.create () in
  let s = create ~dt ~event note in
  events >>= (fun l -> return (List.iter (Event.emit event) l)) >> s

(** Play timed events. *)
let play ~dt (note:'a Note.t) events =
  let event = Event.create () in
  let s = create ~dt ~event note in
  emitter ~dt (Event.emit event) events >> s

let play_midi ~dt (note:'a Note.t) =
  let event = Event.create () in
  let s = create ~dt ~event note in
  let e = ref [] in
  let m = Mutex.create () in
  let _ =
    Thread.create
      (fun () ->
         let open Alsa in
         let seq = Sequencer.create "default" `Input in
         Sequencer.set_client_name seq "Monadic synth";
         let port = Sequencer.create_port seq "Input" [Port_cap_write; Port_cap_subs_write] [Port_type_MIDI_generic] in
         Sequencer.subscribe_read_all seq port;
         Printf.printf "synth started\n%!";
         let add ev =
           Mutex.lock m;
           e := ev :: !e;
           Mutex.unlock m
         in
         (* while false do *)
         while true do
           match (Sequencer.input_event seq).ev_event with
           | Sequencer.Event.Note_on n ->
             let n, v = n.note_note, (float_of_int n.note_velocity /. 127.) in
             Printf.printf "note on: %d at %f\n%!" n v;
             add (`Note_on (n, v))
           | Sequencer.Event.Note_off n ->
             let n = n.note_note in
             Printf.printf "note off: %d\n%!" n;
             add (`Note_off n)
           (* | Sequencer.Event.Controller c -> Printf.printf "controller: %d\n%!" c.controller_value *)
           (* | Sequencer.Event.Pitch_bend c -> Printf.printf "pitch bend: %d\n%!" c.controller_value *)
           (* | _ -> Printf.printf "ignored event\n%!"; *)
           | Sequencer.Event.Unhandled n ->
             Printf.printf "unhandled midi: %d\n%!" n
           | _ -> ()
         done
      ) ()
  in
  let f () =
    Mutex.lock m;
    let l = List.rev !e in
    if l <> [] then Printf.printf "****got events\n%!";
    e := [];
    Mutex.unlock m;
    List.iter (Event.emit event) l
  in
  f >> s

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
