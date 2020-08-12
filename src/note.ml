(** Notes. *)

open Stream

(** A function for creating notes. It takes as arugment the events it can
    recieve, as well as an [on_die] function, which it should call when the note
    has finished playing (we cannot determine this externally in case there is
    some release), and returns a function which plays a note at given frequency
    and volume. *)
type 'event t = event:('event Event.t) -> on_die:(unit -> unit) -> unit -> sample -> float -> sample stream

(** Convert note height into frequency. *)
let freq ?(detune=0.) n = 440. *. (2. ** ((float n +. detune -. 69.) /. 12.))

(** Duration of a note at given tempo. *)
let duration tempo d =
  60. /. tempo *. d

(** Note from an oscillator. *)
let simple f : 'a t =
  fun ~event ~on_die () ->
  let alive = ref true in
  let handler = function
    | `Release -> alive := false; on_die ()
  in
  Event.register event handler;
  let f = f () in
  fun freq vol ->
    bmul (stream_ref alive) (cmul vol (f freq))

let detune ?(cents=return 7.) ?(wet=return 0.5) (note : 'a t) : 'a t =
  fun ~event ~on_die () ->
  let n = note ~event ~on_die () in
  let nd = note ~event ~on_die () in
  fun freq vol ->
    let cents = get cents in
    let wet = get wet in
    let freqd = freq *. (2. ** (cents /. 1200.)) in
    let* d = n freq vol in
    let* w = nd freqd vol in
    return (d +. wet *. w)

(** Add two notes. *)
let add n1 n2 : 'a t =
  fun ~event ~on_die () ->
  let n1 = n1 ~dt ~event ~on_die in
  let n2 = n2 ~dt ~event ~on_die in
  fun freq vol ->
    add (n1 freq vol) (n2 freq vol)

(** Basic (TR-808 type) drum notes. *)
module Drum = struct
  let kick ?on_die () =
    let s = cmul 150. (exponential () (-9.)) >>= sine () in
    let env = adsr () ~a:0.001 ~d:0.1 ~s:0.9 ~sustain:false ~r:0.8 ?on_die () in
    mul env s

  let snare ?on_die ?(a=0.01) ?(d=0.03) ?(s=0.7) ?(r=0.07) ?(lp=80000.) () =
    let env = adsr () ?on_die ~a ~d ~s ~sustain:false ~r ~release:`Exponential () in
    let s = noise () in
    let lpf = Filter.first_order () `Low_pass in
    let* e = env in
    let s = cmul e s in
    s >>= lpf (lp *. e)

  let crash ?on_die () =
    let s = noise () in
    let env = adsr () ?on_die ~a:0.01 ~d:0.05 ~s:0.8 ~sustain:false ~r:0.5 () in
    mul env s

  let closed_hat ?on_die () =
    let s = noise () in
    let env = adsr () ?on_die ~a:0.001 ~d:0.005 ~s:0.3 ~sustain:false ~r:0.01 () in
    let s = s >>= Filter.first_order () `High_pass 4000. in
    mul env s
end

(** Simple note with adsr envelope and volume. *)
let adsr ?a ?d ?s ?r osc : 'a t =
  fun ~event ~on_die () ->
  let g = function
    | Some x -> Some (get x)
    | None -> None
  in
  let env = adsr ~event ~on_die () ?a:(g a) ?d:(g d) ?s:(g s) ?r:(g r) () in
  let osc = osc () in
  fun freq vol ->
    let s = osc freq in
    let s = mul env s in
    cmul vol s
