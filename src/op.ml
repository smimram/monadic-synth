(** Trying to have a state monad too. *)

open Extlib

(** Pure streams. *)
module Stream = struct
  (** Type for time differences. *)
  type dt = float

  (** The stream monad. *)
  type 'a t = dt -> 'a

  (** Return operation of the stream monad. *)
  let return : 'a -> 'a t = fun x dt -> x

  (** Bind operation of the stream monad. *)
  let bind : ('a -> 'b t) -> 'a t -> 'b t =
    fun f x dt -> f (x dt) dt

  let bind2 f x y = bind (fun x -> bind (f x) y) x

  (** Bind a function taking a list of arguments. *)
  let rec bind_list f = function
    | [] -> f []
    | x::l ->
      let f x l = f (x::l) in
      bind (fun x -> bind_list (f x) l) x

  let funct f x = bind (fun x -> return (f x)) x

  let funct2 f x y = bind2 (fun x y -> return (f x y)) x y

  let mul x = bind id x

  (** Current infinitesimal variation of a stream. *)
  let dt : float t = fun dt -> dt
end

type 'a stream = 'a Stream.t

let stream = Stream.return

let dt = Stream.dt

module State = struct
  type 'a t = unit -> 'a

  let return : 'a -> 'a t =
    fun x () -> x

  let bind : ('a -> 'b t) -> 'a t -> 'b t =
    fun f x () -> f (x ()) ()

  let funct f x = bind (fun x -> return (f x)) x

  let mul x = bind id x

  (* let dbind : (('a -> 'b) -> 'c t) -> ('a -> 'b t) -> 'c t = *)
  (* fun f x () -> f (fun t -> x t ()) () *)

  (* This is dual to applicativity *)
  let collect : ('a -> 'b t) -> ('a -> 'b) t =
    fun f () x -> f x ()

  (* Don't use this one. *)
  let run (x : 'a t) : 'a = x ()
end

type 'a t = 'a State.t

type 'a op = 'a State.t

let return = State.return

let ( let+ ) x f = State.bind f x

let ( >> ) x f = let+ () = x in f

let dlaw : 'a op stream -> 'a stream op =
  fun s () dt -> s dt ()

let dbind : ('a -> 'b stream op) -> 'a stream op -> 'b stream op =
  fun f x ->
  let y : 'b stream op stream op = State.funct (Stream.funct f) x in
  let y = State.funct dlaw y in
  let y = State.mul y in
  State.funct Stream.mul y

let ( let* ) (x : 'a stream) (f : 'a -> 'b stream op) : 'b stream op = dbind f (return x)

let collect = State.collect

(** Define a function. *)
let defun f =
  State.collect (fun x -> let* x = x in f x)

let rstream x = return (stream x)

module Ref = struct
  type 'a t = 'a ref

  let create x : 'a t op = fun () -> ref x

  let get (r : 'a t) : 'a op = fun () -> !r

  let set (r : 'a t) x : unit op = fun () -> r := x
end

let id : (float stream -> float stream) t =
  let+ _ = Ref.create 0 in
  State.collect
    (fun x ->
       let+ _ = Ref.create 0 in
       let* x = x in
       return (stream x)
    )

let nop () = return ()

(** {2 Arithmetic} *)

(** Create a constant stream. *)
let cst x = stream x

(** The constantly zero stream. *)
let blank = cst 0.

(** Multiply two streams. *)
let mul x y = Stream.funct2 ( *. )

(** Multiply a stream by a boolean (interpreted as 0 / 1 for false / true). *)
let bmul b x =
  if b then return x else return 0.

(** Amplify a stream. *)
let amp = mul

(** Add two streams. *)
let add x y = Stream.funct2 (+.)

(** Add a list of streams. *)
let rec mix =
  Stream.bind_list (fun ss -> stream (List.fold_left (+.) 0. ss))

(** Subtract streams. *)
let sub = Stream.funct2 ( -. )

(** Clip a stream in the interval [-1., 1.]. *)
let clip = Stream.funct (fun x -> max (-1.) (min 1. x))

let soft_clip =
  Stream.funct
    (fun x ->
       if x <= -1. then (-2.)/.3.
       else if x >= 1. then 2./.3.
       else x-.x*.x*.x/.3.
    )

(** Stretch a parameter between 0 and 1 to be between given bounds. *)
let stretch ?(mode=`Linear) ?(min=0.) ?(max=1.) =
  Stream.funct
    (
      let d = max -. min in
      match mode with
      | `Linear -> fun x -> x *. d +. min
      | `Logarithmic ->
        fun x ->
          let x = (10. ** x -. 1.) /. 9. in
          x *. d +. min
    )

(** Inverse of [stretch]. *)
let unstretch ?(mode=`Linear) ?(min=0.) ?(max=1.) =
  Stream.funct
    (
      let d = max -. min in
      match mode with
      | `Linear -> fun x -> (x -. min) /. d
      | `Logarithmic ->
        fun x ->
          let x = (x -. min) /. d in
          log10 (x *. 9. +. 1.)
    )

(** Convert octave numbers to multiplicative coefficient for frequency. *)
let octaves = Stream.funct (Float.pow 2.)

(** Number of samples in a given amount of time. *)
let samples t =
  Stream.bind (fun dt -> stream (round (t /. dt))) dt

(** {2 Time} *)

(** Integrate a stream. *)
let integrate ?(on_reset=nop) ?(init=0.) ?(periodic=false) () : (float stream -> float stream) t =
  let+ y = Ref.create init in
  defun (fun x ->
      let* dt  = dt in
      let+ ans = Ref.get y in
      Ref.set y (!y +. x *. dt) >>
      (
        if periodic && ans >= 1. then
          Ref.set y (ans -. 1.) >> on_reset ()
        else
          return ()
      ) >> rstream ans
    )

(** Current time. *)
let now =
  let+ i = integrate () in
  return (i (cst 1.))

(** Current time for a periodic function. *)
let periodic ?(init=0.) ?on_reset () =
  integrate ~periodic:true ~init ?on_reset ()

(** Create a stream from timed events, supposed to be sorted. If tempo is
    not specified, time is assumed in seconds (otherwise, in notes). *)
let timed ?tempo ?(loop=false) l =
  let l =
    match tempo with
    | Some tempo -> List.map (fun (t,e) -> 60. /. tempo *. t, e) l
    | None -> l
  in
  let l0 = l in
  let+ l = Ref.create l in
  let+ toff = Ref.create 0. in
  let+ now = now in
  let* time = now in
  let rec aux ans =
    match !l with
    | (t,e) :: tl when t +. !toff <= time ->
      let ans = e::ans in
      Ref.set l tl >>
      aux ans
    | [] ->
      if loop then
        (
          Ref.set toff time >>
          Ref.set l l0 >>
          rstream ans
        )
      else
        rstream ans
    | _ -> rstream ans
  in
  aux []

(*
(** {2 Control} *)

(** When a stream becomes true. *)
let activates () =
  let prev = ref false in
  fun b ->
    let p = !prev in
    prev := b;
    return (not p && b)

(** When a stream changes value. *)
let changes () =
  let first = ref true in
  let prev = ref false in
  fun b ->
    if !first then
      (
        first := false;
        prev := b;
        return false
      )
    else
      (
        let p = !prev in
        prev := b;
        return ((not p && b) || (p && not b))
      )

(** Zero-crossing: is true when the stream was negative and becomes positive. *)
let zc () =
  let activates = activates () in
  fun s ->
    s >>= (fun x -> return (x >= 0.)) >>= activates

(** Check whether we are at a particular instant. *)
let at () =
  let now = now () in
  let activates = activates () in
  fun (time:float) ->
    let* t = now in
    activates (t >= time)

(** Check whether we are after a particular instant. *)
let after () =
  let now = now () in
  fun time ->
    let* t = now in
    return (t >= time)

(** Generate an event at a given frequency. *)
let frequently () =
  let b = ref false in
  let on_reset () = b := true in
  let p = periodic ~on_reset () in
  fun freq ->
    p freq >>= drop >>
    let* cond = Ref.get b in
    if cond then (b := false; return true)
    else return false

(** Generate an event every period of time. *)
let every () =
  let f = frequently () in
  fun time -> f (1. /. time)

(** Whether this is the first sample of the stream. *)
let is_first () =
  let first = ref true in
  fun () ->
    let ans = !first in
    first := false;
    return ans

(** Execute an action when a stream is true. *)
let on f b =
  if b then return (f ()) else return ()

(** Sample when a condition is true and hold the sample the rest of the time. *)
let sample_and_hold () =
  let r = ref None in
  fun b x ->
    let* _ = dt in
    if b || !r = None then r := Some x;
    return (Option.get !r)

(** Execute a function when a stream change its value. *)
let on_change ?(first=false) f =
  let old = ref None in
  fun x ->
    match !old with
    | Some x0 when x0 = x -> return x
    | Some _ -> old := Some x; f x; return x
    | None -> old := Some x; if first then f x; return x

let fallback (x:'a t) (y:'a t) b : 'a t =
  if b then x else y

let fallblank x b = fallback x blank b

(** Generate a random value at given frequency. *)
let random () =
  let x = ref 0. in
  let frequently = frequently () in
  fun ?(min=0.) ?(max=1.) freq ->
    frequently freq >>= on (fun () -> x := Random.float (max -. min) +. min) >> Ref.get x

*)

































let saw =
  let+ p = periodic () in
  collect (fun freq ->
    let* x = p freq in
    rstream (2. *. x -. 1.)
    )

let stereo x =
  Stream.bind (fun x -> stream (x, x)) x

module Output = struct
  open Output

  let play s =
    play (State.run s)
end
