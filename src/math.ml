(** Generic arithmetical operations .*)

open Extlib

let clip x = max (-1.) (min 1. x)

(** Stretch a parameter between 0 and 1 to be between given bounds. *)
let stretch ?(mode=`Linear) ?(min=0.) ?(max=1.) =
  let d = max -. min in
  match mode with
  | `Linear -> fun x -> x *. d +. min
  | `Logarithmic ->
    fun x ->
      let x = (10. ** x -. 1.) /. 9. in
      x *. d +. min

(** Inverse of [stretch]. *)
let unstretch ?(mode=`Linear) ?(min=0.) ?(max=1.) =
  let d = max -. min in
   match mode with
  | `Linear -> fun x -> (x -. min) /. d
  | `Logarithmic ->
    fun x ->
      let x = (x -. min) /. d in
      log10 (x *. 9. +. 1.)

(** Oscillators with period 1., first going up, starting from 0. *)
module Osc = struct
  (** Change periodic time (between 0. and 1.) so that width becomes as
      specified. *)
  let width width =
    if width = 0.5 then fun t -> t
    else fun t ->
      if t <= 0.5 then t /. 0.5 *. width
      else (t -. 0.5) /. 0.5 *. (1. -. width) +. width

  (** Tablulate a function at given frequency. *)
  let tabulate f freq =
    let freqn = Float.to_int freq in
    let a = Array.init freqn (fun i -> f (float i /. freq)) in
    fun t ->
      a.(Float.to_int (t *. freq) mod freqn)

  let sine t = sin (2. *. Float.pi *. t)

  let triangle t =
    (*
    if t <= 0.5 then 4. *. t -. 1.
    else 1. -. 4. *. (t -. 0.5)
    *)
    if t <= 0.25 then
      4. *. t
    else if t <= 0.75 then
      (* 1. -. 4. *. (t -. 0.25) *)
      4. *. t
    else
      (* 4. *. (t -. 0.75) -. 1. *)
      4. *. (t -. 1.)

  let square t = if t <= 0.5 then 1. else -1.

  let saw t =
    if t <= 0.5 then 2. *. t
    else 2. *. (t -. 1.5)

  let noise _ = Random.float ~min:(-1.) 1.
end
