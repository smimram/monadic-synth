(** Interfacing with MIDI keyboards and controllers. *)

open Stream.Operations

type event =
  [ `Note_on of int * float
  | `Note_off of int
  | `Controller of int * float
  | `Pitch_bend of int * float
  | `Program_change of int * int
  | `Nop (** Do not do anything. This is useful to extend repeated patterns. *)
  ]

(** A stream of MIDI events. *)
type stream = event list Stream.t

type t =
  {
    mutex : Mutex.t;
    thread : Thread.t;
    map : int -> event -> (int * event);
    handlers : (int -> event -> unit) list ref;
    send : int -> event -> unit
  }

let create ?(synchronous=false) () =
  let mutex = Mutex.create () in
  let handlers = ref [] in
  let seq = Alsa.Sequencer.create "default" `Duplex in
  let thread =
    Thread.create
      (fun () ->
         let open Alsa in
         Sequencer.set_client_name seq "Monadic synth";
         let port = Sequencer.create_port seq "Input" [Port_cap_write; Port_cap_subs_write] [Port_type_MIDI_generic] in
         Sequencer.subscribe_read_all seq port;
         Sequencer.subscribe_write_all seq port;
         Printf.printf "synth started\n%!";
         let add c e =
           Mutex.lock mutex;
           List.iter (fun f -> f c e) !handlers;
           Mutex.unlock mutex
         in
         while true do
           match (Sequencer.input_event seq).ev_event with
           | Sequencer.Event.Note_on n ->
             let c, n, v = n.note_channel, n.note_note, float_of_int n.note_velocity /. 127. in
             Printf.printf "note on  (%d): %d at %f\n%!" c n v;
             add c (`Note_on (n, v))
           | Sequencer.Event.Note_off n ->
             let c, n = n.note_channel, n.note_note in
             Printf.printf "note off (%d): %d\n%!" c n;
             add c (`Note_off n)
           | Sequencer.Event.Controller c ->
             let c, n, v = c.controller_channel, c.controller_param, float c.controller_value  /. 127. in
             Printf.printf "controller (%d): %d at %f\n%!" c n v;
             add c (`Controller (n, v))
           | Sequencer.Event.Pitch_bend c ->
             let c, n, v = c.controller_channel, c.controller_param, float c.controller_value  /. 127. in
             Printf.printf "pitch bend (%d): %d at %f\n%!" c n v;
             add c (`Pitch_bend (n, v))
           | Sequencer.Event.Program_change c ->
             let c, n, v = c.controller_channel, c.controller_param, c.controller_value in
             Printf.printf "program change (%d): %d at %d\n%!" c n v;
             add c (`Program_change (n, v))
           (* | _ -> Printf.printf "ignored event\n%!"; *)
           | Sequencer.Event.Unhandled n ->
             Printf.printf "unhandled midi: %d\n%!" n
           | _ -> ()
         done
      ) ()
  in
  let send chan e =
    let open Alsa in
    let e = match e with
      | `Note_on (n, v) ->
        let v = int_of_float (v *. 127.) in
        (* Printf.printf "note on: %d at %d\n%!" n v; *)
        Sequencer.Event.Note_on {Sequencer.Event. note_channel = chan; note_note = n; note_velocity = v; note_off_velocity = v; note_duration = 1000}
      | _ -> failwith "TODO"
    in
    Sequencer.output_event seq e
  in
  {
    mutex;
    thread;
    map = (fun c e -> (c,e));
    handlers;
    send;
  }

(** Register a handler of midi events. *)
let register midi h =
  let h c e =
    let c, e = midi.map c e in
    h c e
  in
  midi.handlers := h :: !(midi.handlers)

(** Map a function on all events. *)
let map midi f =
  let map c e =
    let c, e = midi.map c e in
    f c e
  in
  { midi with map }

(** Create a stream of midi events. *)
let events ?channel midi : stream =
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
  let s () =
    Mutex.lock m;
    let ee = !nn in
    nn := [];
    Mutex.unlock m;
    ee
  in
  Stream.seq s

let send midi = midi.send

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
