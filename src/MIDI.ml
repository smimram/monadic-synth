open Stream.Common

type event =
  [ `Note_on of int * float
  | `Note_off of int
  | `Controller of int * float
  | `Pitch_bend of int * float
  | `Program_change of int * int
  ]

type t =
  {
    mutex : Mutex.t;
    mutable thread : Thread.t option;
    mutable handlers : (int -> event -> unit) list;
  }

let create ?(synchronous=false) () =
  let midi =
    {
      mutex = Mutex.create ();
      thread = None;
      handlers = [];
    }
  in
  let thread =
    Thread.create
      (fun () ->
         let open Alsa in
         let seq = Sequencer.create "default" `Input in
         Sequencer.set_client_name seq "Monadic synth";
         let port = Sequencer.create_port seq "Input" [Port_cap_write; Port_cap_subs_write] [Port_type_MIDI_generic] in
         Sequencer.subscribe_read_all seq port;
         Printf.printf "synth started\n%!";
         let add e =
           Mutex.lock midi.mutex;
           List.iter (fun f -> f (fst e) (snd e)) midi.handlers;
           Mutex.unlock midi.mutex
         in
         while true do
           match (Sequencer.input_event seq).ev_event with
           | Sequencer.Event.Note_on n ->
             let c, n, v = n.note_channel, n.note_note, float_of_int n.note_velocity /. 127. in
             Printf.printf "note on  (%d): %d at %f\n%!" c n v;
             add (c, `Note_on (n, v))
           | Sequencer.Event.Note_off n ->
             let c, n = n.note_channel, n.note_note in
             Printf.printf "note off (%d): %d\n%!" c n;
             add (c, `Note_off n)
           | Sequencer.Event.Controller c ->
             let c, n, v = c.controller_channel, c.controller_param, float c.controller_value  /. 127. in
             Printf.printf "controller (%d): %d at %f\n%!" c n v;
             add (c, `Controller (n, v))
           | Sequencer.Event.Pitch_bend c ->
             let c, n, v = c.controller_channel, c.controller_param, float c.controller_value  /. 127. in
             Printf.printf "pitch bend (%d): %d at %f\n%!" c n v;
             add (c, `Pitch_bend (n, v))
           | Sequencer.Event.Program_change c ->
             let c, n, v = c.controller_channel, c.controller_param, c.controller_value in
             Printf.printf "program change (%d): %d at %d\n%!" c n v;
             add (c, `Program_change (n, v))
           (* | _ -> Printf.printf "ignored event\n%!"; *)
           | Sequencer.Event.Unhandled n ->
             Printf.printf "unhandled midi: %d\n%!" n
           | _ -> ()
         done
      ) ()
  in
  midi.thread <- Some thread;
  midi

let register midi h =
  midi.handlers <- h :: midi.handlers

(** Create a stream of midi events. *)
let events ?channel midi =
  let m = Mutex.create () in
  let nn = ref [] in
  let h c e =
    if channel = None || Some c = channel then
      (
        Mutex.lock m;
        nn := e :: !nn;
        Mutex.unlock m
      )
  in
  register midi h;
  fun () ->
    Mutex.lock m;
    let ee = !nn in
    nn := [];
    Mutex.unlock m;
    ee

(** The value of a specific controller. *)
let controller midi ?channel number ?mode ?min ?max init =
  let stretch = Stream.stretch ?mode ?min ?max in
  let m = Mutex.create () in
  let x = ref init in
  let h c e =
    if channel = None || Some c = channel then
      match e with
      | `Controller (n,v) when n = number ->
        Mutex.lock m;
        x := stretch v;
        Mutex.unlock m
      | _ -> ()
  in
  register midi h;
  Stream.stream_ref x

(** The value of a toggle controller. *)
let toggle midi ?channel ?(init=false) number =
  controller midi ?channel number (if init then 1. else 0.) >>=
  (fun x -> return (x <> 0.))
