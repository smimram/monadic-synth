(** Streams. *)

open Extlib

(** Type for samples. *)
type sample = float

(** {2 The stream monad} *)

(** Type for time differences. *)
type dt = float

(** The stream monad. *)
type 'a t = dt -> 'a

(** Alias for the stream monad. *)
type 'a stream = 'a t

(** Return operation of the stream monad. *)
let return : 'a -> 'a t = fun x _ -> x

(** Bind operation of the stream monad. *)
let bind : ('a -> 'b t) -> 'a t -> 'b t =
  fun f x dt -> f (x dt) dt

(** The stream monad is applicative. *)
let apply : ('a -> 'b) t -> 'a t -> 'b t =
  fun f x dt -> f dt (x dt)

(** Bind two arguments. *)
let bind2 f x y =
  bind (fun x -> bind (f x) y) x

(** Bind three arguments. *)
let bind3 f x y z =
  bind (fun x -> bind (fun y -> bind (f x y) z) y) x

(** Bind four arguments. *)
let bind4 f x y z t =
  bind (fun x -> bind (fun y -> bind (fun z -> bind (f x y z) t) z) y) x

(** Bind the first of two arguments. *)
let bind1_2 f x y =
  bind (fun x -> f x y) x
  

(** Bind a function taking a list of arguments. *)
let rec bind_list f = function
  | [] -> f []
  | x::l ->
    let f x l = f (x::l) in
    bind (fun x -> bind_list (f x) l) x

(** Functoriality of the stream monad. *)
let funct : ('a -> 'b) -> 'a t -> 'b t =
  fun f x -> bind (fun x -> return (f x)) x

(** Functoriality in two arguments of the stream monad. *)
let funct2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t =
  fun f x y -> bind2 (fun x y -> return (f x y)) x y

(** Strength of the stream monad. *)
let prod : 'a t -> 'b t -> ('a * 'b) t =
  fun x y dt -> (x dt, y dt)

(** Current infinitesimal variation of a stream. *)
let dt : float t =
  fun dt -> dt

(** Current value of a stream (this function might be removed in the future). *)
let get : 'a t -> 'a =
  fun f -> f 0.

(** Notations for usual operations of the stream monad. You usually want to open
    this module when dealing with streams. *)
(* Nice explanation of monadic syntax at https://jobjo.github.io/2019/04/24/ocaml-has-some-new-shiny-syntax.html *)
module Operations = struct
  (** Return. *)
  let return = return

  (** Bind. *)
  let ( >>= ) x f = bind f x

  (** Bind with unit result. *)
  let ( >> ) x f = x >>= (fun () -> f)

  (** Functoriality. *)
  let ( <$> ) = funct

  (** Applicativity. *)
  let ( <*> ) = apply

  (** Bind. *)
  let ( let* ) x f = bind f x

  (** Strength. *)
  let ( and* ) = prod
end

include Operations

(** {2 Pure operations} *)

(** Forget the result of the stream (this is [ignore] for streams). *)
let drop _ = return ()

(** Map a function on every sample of a stream. *)
let map f x = return (f x)

(** Iterate a function on every sample of a stream. *)
let iter f = map (fun x -> f x; x)

(** Create a stream from a function indicating its value at each call. *)
let seq f =
  let* _ = dt in
  return (f ())

(** Value of the stream at previous instant. *)
let prev (x0:'a) =
  let prev = ref x0 in
  fun x ->
    let ans = !prev in
    prev := x;
    return ans

(** Set the first values of a stream. This is useful to allocate the buffers in
    operators which have some. *)
let initialize l =
  let l = ref l in
  fun x ->
    match !l with
    | [] -> return x
    | x::l' -> l := l'; return x

(** Stream duplication. Once the left part has been evaluated, the right part
    can be used as many times as wanted. This has to be used if you need to use a
    stream more than once, in order to avoid each copy asking for a different
    sample. *)
let dup () =
  let x = ref None in
  fun s ->
    (let* y = s in return (x := Some y)),
    (fun _ -> try Option.get !x with _ -> failwith "Invalid evaluation order in dup.")

(** Stream the current value of a reference. *)
let stream_ref x = seq (fun () -> !x)

(** Operations on lists of streams. *)
module StreamList = struct
  let rec iter f = function
    | [] -> return ()
    | s::l ->
      let* x = s in
      f x; iter f l

  let rec fold_left f x0 = function
    | [] -> return x0
    | s::l ->
      let* x = s in
      fold_left f (f x0 x) l
end

(** Event hubs. On those, handlers can be registered and will be called each
    time a new event is emitted. *)
module Event = struct
  (** An event hub. *)
  type 'a t = ('a -> unit) list ref

  (** Create an event hub. *)
  let create () : 'a t = ref []

  (** Register a handler on the hub. *)
  let register (h:'a t) f =
    h := f :: !h

  (** Emit an event. *)
  let emit (h:'a t) e =
    List.iter (fun f -> f e) !h

  (** Stream which emits lists of events. *)
  let emitter h e =
    return (List.iter (emit h) e)

  (** Merge two hubs. *)
  let merge h1 h2 = ref (!h1 @ !h2)

  (** Forwrad the events of a hub to a second one. *)
  let forward src dst =
    let f e = emit dst e in
    register src f

  let may_map f h =
    let ans = create () in
    let f' e =
      match f e with
      | Some e -> emit ans e
      | None -> ()
    in
    register h f'
end

(** {2 Arithmetic} *)

(** Create a constant stream. *)
let cst x = return x

(** The constantly zero stream. *)
let blank = cst 0.

(** Multiply a stream by a boolean (interpreted as 0 / 1 for false / true). *)
let bmul b x =
  if b then return x else return 0.

(** Switched multiplication by a constant: if the first is 0, the second stream
    is not evaluated. *)
let scmul x s =
  if x = 0. then return 0.
  else
    let* y = s in
    return (x *. y)

let smulc s y =
  let* x = s in
  if x = 0. then return 0.
  else return (x *. y)

(** Multiply two streams. *)
let mul x y = return (x *. y)

(** Amplify a stream. *)
let amp = mul

(** Add two streams. *)
let add x y = return (x +. y)

(** Add a list of streams. *)
let mix ss =
  return (List.fold_left (+.) 0. ss)

(** Subtract streams. *)
let sub = funct2 ( -. )

(** Clip a stream in the interval [-1., 1.]. *)
let clip x = return (Math.clip x)

(** Softer (and more distorted) version of the [clip] function. *)
let soft_clip x =
  if x <= -1. then (-2.)/.3.
  else if x >= 1. then 2./.3.
  else x-.x*.x*.x/.3.

(** Convert octave numbers to multiplicative coefficient for frequency. *)
let octaves x =
  return (Float.pow 2. x)

(** Number of samples in a given amount of time. *)
let samples t =
  let* dt = dt in
  return (round (t /. dt))

(** {2 Time} *)

(** Integrate a stream. *)
let integrate ?(kind=`Euler) ?(event=Event.create ()) ?(on_reset=nop) ?(init=0.) ?(periodic=false) () =
  let y = ref init in
  let handler = function
    | `Reset -> y := init; on_reset ()
    | `Set x -> y := x
  in
  Event.register event handler;
  let return ans =
    if periodic && !y >= 1. then (y := !y -. 1.; on_reset ());
    return ans
  in
  match kind with
  | `Euler ->
    fun x ->
      let* dt = dt in
      let ans = !y in
      y := !y +. x *. dt;
      return ans
  | `Trapezoidal ->
    let u = ref 0. in
    fun x ->
      let* dt = dt in
      ignore dt;
      let ans = !y in
      y := !u +. x /. 2.;
      u := !u +. x;
      return ans

(** Current time. *)
let now ?event () : sample t =
  integrate ?event () 1.

(** Current time for a periodic function. *)
(* TODO: implement periodic with events *)
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
  let l = ref l in
  let toff = ref 0. in
  let now = now () in
  let* time = now in
  let rec aux ans =
    match !l with
    | (t,e) :: tl when t +. !toff <= time ->
      let ans = e::ans in
      l := tl;
      aux ans
    | [] ->
      if loop then
        (
          toff := time;
          l := l0;
          return ans
        )
      else
        return ans
    | _ -> return ans
  in
  aux []

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
    let* _ = dt in
    if !b then (b := false; return true)
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

(** Compute a source a lower sampling rate than the master sampling rate. *)
let downsample freq s =
  let r = ref None in
  let on_reset () = r := Some (s (1. /. freq)) in
  let p = periodic ~on_reset () in
  let* _ = p freq in
  if !r = None then r := Some (get s);
  return (Option.get !r)

(** Execute a function when a stream change its value. *)
let on_change ?(first=false) f =
  let old = ref None in
  fun x ->
    match !old with
    | Some x0 when x0 = x -> return x
    | Some _ -> old := Some x; f x; return x
    | None -> old := Some x; if first then f x; return x

(** Switch between two streams depending on a boolean. *)
let switch x y b =
  if b then x else y

(** Generate a random value at given frequency. *)
let random () =
  let x = ref 0. in
  let frequently = frequently () in
  fun ?(min=0.) ?(max=1.) freq ->
    frequently freq >>= on (fun () -> x := Random.float (max -. min) +. min) >> stream_ref x

(** Operations with samples as unit time. *)
module Sample = struct
  let every () =
    let t = ref 0 in
    fun n ->
      let* _ = dt in
      incr t;
      let b = !t >= n in
      if b then t := !t - n;
      return b

  (** Convolution with an impulse response. *)
  let convolve a =
    let len = Array.length a in
    let prev = Array.make len 0. in
    let n = ref 0 in
    fun x ->
      let* _ = dt in
      prev.(!n) <- x;
      let ans = ref 0. in
      for i = 0 to len - 1 do
        ans := !ans +. a.(i) *. prev.((!n - i + len) mod len);
      done;
      incr n;
      if !n = len then n := 0;
      return !ans

  (** Ringbuffers. *)
  module Ringbuffer = struct
    (** A ringbuffer. *)
    type t =
      {
        mutable buffer : sample array; (* Buffer. *)
        mutable pos : int; (* Write cursor. *)
      }

    let create () =
      {
        buffer = [||];
        pos = 0;
      }

    (** Ensure that the buffer can hold this amount of data. *)
    let prepare ?init r size =
      let l0 = Array.length r.buffer in
      let l = size + 2 in
      if l0 < l then
        let buf = r.buffer in
        r.buffer <- Array.make l 0.;
        Array.blit buf 0 r.buffer 0 l0;
        match init with
        | None -> ()
        | Some f -> for i = l0 to l - 1 do r.buffer.(i) <- f () done

    (** Number of sample that the buffer can hold. *)
    let size r =
      Array.length r.buffer - 1

    (** Advance the write cursor by one sample. *)
    let advance r =
      r.pos <- r.pos + 1;
      if r.pos >= Array.length r.buffer then r.pos <- 0

    let past r delay =
      let delay = delay + 1 in
      assert (0 <= delay && delay <= size r);
      let prev = r.pos - delay in
      let prev = if prev < 0 then prev + Array.length r.buffer else prev in
      r.buffer.(prev)

    let write r x =
      r.buffer.(r.pos) <- x;
      advance r
  end

  (** Delay the signal by given amount of samples. *)
  let delay () =
    let r = Ringbuffer.create () in
    fun delay x ->
      Ringbuffer.prepare r delay;
      Ringbuffer.write r x;
      let x = Ringbuffer.past r delay in
      return x

  (** A fixed delay which inputs the sample at t-delay, passes it to a function,
      and writes the result. Useful for implementing the recursive part of
      filters. *)
  let rec_delay () =
    let r = Ringbuffer.create () in
    fun delay f ->
      Ringbuffer.prepare r delay;
      let* y = f (Ringbuffer.past r delay) in
      Ringbuffer.write r y;
      return y

  (** Comb filter. *)
  (* https://ccrma.stanford.edu/~jos/pasp/Feedforward_Comb_Filters.html *)
  (* https://ccrma.stanford.edu/~jos/pasp/Feedback_Comb_Filters.html *)
  let comb ?(kind=`Feedback) () =
    match kind with
    | `Feedback ->
      let d = rec_delay () in
      fun m a x ->
        d m (fun y' -> return (x -. a *. y'))
    | `Feedforward ->
      let d = delay () in
      fun m a x ->
        let* x' = d m x in
        return (x +. a *. x')

  (** All-pass filter. *)
  (* https://ccrma.stanford.edu/~jos/Delay/Schroeder_Allpass_Filters.html and
     https://ccrma.stanford.edu/~jos/pasp/Schroeder_Reverberators.html *)
  let schroeder_allpass () =
    let dx = delay () in
    let dy = rec_delay () in
    fun m g x ->
      dy m (fun y' -> dx m x >>= (fun x' -> return (x'+.g*.(y'-.x))))

  (** Fast Fourrier transform. *)
  let fft a off len =
    let ( +~ ) = Complex.add in
    let ( -~ ) = Complex.sub in
    let ( *~ ) = Complex.mul in
    let a = Array.sub a off len in
    let n = len in
    (* Compute log2 n *)
    let p =
      let n = ref n in
      let p = ref 0 in
      while !n land 1 = 0 do
        n := !n lsr 1;
        incr p
      done;
      !p
    in
    (* Ensure that n is a power of 2. *)
    assert (n = 1 lsl p);
    let exp k n =
      let theta = (-2.) *. Float.pi *. float k /. float n in
      Complex.polar 1. theta
    in
    (* TODO: optimize *)
    let rec aux a =
      let n = Array.length a in
      if n = 1 then a else
        let e = Array.init (n/2) (fun k -> a.(2*k)) in
        let o = Array.init (n/2) (fun k -> a.(2*k+1)) in
        let e = aux e in
        let o = aux o in
        let ans1 = Array.init (n/2) (fun k -> e.(k) +~ exp k n *~ o.(k)) in
        let ans2 = Array.init (n/2) (fun k -> e.(k) -~ exp k n *~ o.(k)) in
        Array.append ans1 ans2
    in
    Array.map (Complex.cmul (1./.float n)) (aux a)

  let ifft a =
    let n = Array.length a in
    let a = Array.map (Complex.cmul (float n)) a in
    let a = Array.init n (fun k -> if k = 0 then a.(0) else a.(n-k)) in
    fft a 0 n
end

(** {2 Oscillators} *)

let saw () : float -> sample t =
  let p = periodic ~init:0.5 () in
  fun freq ->
    let* t = p freq in
    return (Math.Osc.saw t)

let triangle () =
  let p = periodic ~init:0.25 () in
  fun freq ->
    let* t = p freq in
    return (Math.Osc.triangle t)

let sine () : float -> sample t =
  let p = periodic () in
  fun freq ->
    let* t = p freq in
    return (Math.Osc.sine t)

let square () =
  let p = periodic () in
  fun ?(width=0.5) freq ->
    let* t = p freq in
    let t = Math.Osc.width width t in
    return (Math.Osc.square t)

let noise () = seq (fun () -> Random.float ~min:(-1.) 1.)

(** Play a sample stored in a buffer at various speeds. *)
let sampler ?(interpolation=`Closest) ?(freq=1.) buf =
  let p = periodic () in
  let buflen = Array.length buf in
  let fbuflen = float buflen in
  let f =
    match interpolation with
    | `Closest ->
      (fun t ->
        let n = int_of_float (fbuflen *. t +. 0.5) in
        return buf.(n))
  in
  fun freq' ->
    let* dt = dt in
    let* t = p (freq' /. (freq *. dt *. fbuflen)) in
    f t

module Spectral = struct
  module Window = struct
    let hamming buf =
      let n = float (Array.length buf) in
      let f k = 0.54 -. 0.46 *. cos (2.*.Float.pi*.float k/.(n-.1.)) in
      Array.mapi (fun k x -> Complex.cmul (f k) x) buf
  end

  let sampler ?freq ?interpolation buf =
    let buf = Array.map Complex.re (Sample.ifft buf) in
    sampler ?interpolation ?freq buf

  (*
  (* See http://zynaddsubfx.sourceforge.net/doc/PADsynth/PADsynth.htm *)
  let pad ?(bandwidth=40.) ?(harmonics=Array.init 64 (fun i -> 1./.(float i)*.(if i mod 2 = 0 then 2. else 1.))) () =
    let buflen = 1 lsl 17 in
    let f0 = 512. in
    let nharmonics = Array.length harmonics in
    (* Convert bandwidth from cents to Hz. *)
    let bandwidth = 2. ** (bandwidth /. 1200.) in
    (* Harmonics profile. *)
    let profile k bw =
      let x = k /. bw in
      exp (-. x *. x) /. bw
    in
    let spectrum = Array.make (buflen/2) 0. in
    let band0 = f0 *. float nharmonics *. dt in
    for i = 0 to nharmonics - 1 do
      let h = harmonics.(i) in
      (* Bandwidth in Hz *)
      let bandwidth = bandwidth *. f0 *. float i in
      for j = 0 to buflen / 2 - 1 do
        let a = profile (float i /. float nharmonics -. band0) bandwidth in
        spectrum.(j) <- spectrum.(j) +. a *. h
      done
    done;
    let spectrum = Array.map (fun x -> Complex.polar x (Random.float 1.)) spectrum in
    (* We want a symmetric spectrum. *)
    let spectrum = Array.append spectrum (Array.init (buflen/2) (fun k -> if k = 0 then spectrum.(0) else spectrum.(buflen/2-k))) in
    sampler ~freq:f0 spectrum

  let harmonics ~dt ?(harmonics=8) ?(shape=`Gaussian) ?(width=0.01) ?(decay=`Exponential 100.) () =
    (* Frequency to synthesize. *)
    let f0 = 500. in
    (* IFFT length. *)
    let buflen = 1 lsl 18 in
    (* An harmonic peak centered around f0. *)
    let h f0 =
      match shape with
      | `Dirac ->
        let delta = 1. /. (2. *. float buflen *. dt) in
        (fun f -> if f0 -. delta <= f && f < f0 +. delta then 1. else 0.)
      | `Triangle ->
        let delta = f0 *. width /. 2. in
        (fun f ->
          if f <= f0 -. delta || f >= f0 +. delta then 0. else
            if f <= f0 then
              (f -. (f0 -. delta)) /. delta
            else
              (f0 -. f) /. delta +. 1.
        )
      | `Gaussian ->
        (fun f ->
          let t = f -. f0 in
          exp (-. t *. t /. (f0 *. width))
        )
    in
    let decay =
      match decay with
      | `Exponential c ->
        (fun k -> exp ((-.c) *. float k))
    in
    let hl = List.init harmonics (fun k f -> decay k *. h (float (k+1) *. f0) f) in
    (* Harmonic intensity at frequency f. *)
    let h f = List.fold_left (fun x h -> x +. h f) 0. hl in
    let buf = Array.init (buflen/2) (fun k ->
      let f = float k /. (float buflen *. dt) in
      let a = h f in
      let t = Random.float (2.*.pi) in
      Complex.polar a t
    )
    in
    let buf = Array.append buf (Array.init (buflen/2) (fun k -> if k = 0 then buf.(0) else buf.(buflen/2-k))) in
    sampler ~freq:f0 buf
   *)
end

(** Generic oscillator. *)
let osc () =
  let p = periodic () in
  fun ?(width=0.5) kind freq ->
    let* t = p freq in
    let t = Math.Osc.width width t in
    let f =
      match kind with
      | `Sine -> Math.Osc.sine
      | `Saw -> Math.Osc.saw
      | `Square -> Math.Osc.square
      | `Triangle -> Math.Osc.triangle
      | `Noise -> Math.Osc.noise
    in
    return (f t)

(** Frequency modulation synthesis. *)
let fm ?(carrier=`Sine) ?(modulator=`Sine) () =
  let carrier = osc () carrier in
  let modulator = osc () modulator in
  fun ?(ratio=1.) depth freq ->
    let* m = modulator (ratio *. freq) in
    carrier (freq +. depth *. m)

let random_zero () =
  let x = ref 0. in
  fun ?(attraction=500.) speed ->
    let* dt = dt in
    let attraction = attraction *. dt in
    let speed = speed *. dt in
    let ans = !x in
    let to_zero = Random.float 1. < attraction in
    let d = (Random.float 2. -. 1.) *. speed in
    let d = if to_zero && (!x *. d) > 0. then -.d else d in
    x := ans +. d;
    return ans

(*
let karplus_strong () ?(filter=return) freq =
  let n = samples ~dt (1. /. freq) in
  let buflen = n+1 in
  let buf = Array.init buflen (fun i -> if i = 0 then 0. else noise ()) in
  let pos = ref 0 in
  let get k = if k < 0 then buf.(k+buflen) else buf.(k) in
  let ks () =
    let k = !pos - n in
    let x = get (k - 1) in
    let y = get k in
    (x +. y) /. 2.
  in
  let write x =
    buf.(!pos) <- x;
    incr pos;
    if !pos = buflen then pos := 0;
    return x
  in
  ks >>= filter >>= write
  *)

let karplus_strong ?filter () =
  let r = Sample.Ringbuffer.create () in
  let prev = prev 0. in
  let average x =
    let* y = prev x in
    return ((x +. y) /. 2.)
  in
  let filter = Option.value ~default:average filter in
  fun freq ->
    let* n = samples (1. /. freq) in
    let init () = get (noise ()) in
    Sample.Ringbuffer.prepare r ~init n;
    let ans = Sample.Ringbuffer.past r n in
    let* x = filter ans in
    Sample.Ringbuffer.write r x;
    return ans

(** {2 Envelopes} *)

module Envelope = struct
  let apply e x =
    let* e = e in
    return (e *. x)

  (** ADSR (Attack / Decay / Sustain / Release) envelope. *)
  let adsr ?(event=Event.create ()) ?(on_die=ignore) () =
    let state = ref `Attack in
    let log2 = log 2. in
    let amp = ref 0. in
    let die () = state := `Dead; on_die (); amp := 0.; return 0. in
    let integ amp' =
      let* dt = dt in
      let ans = !amp in
      amp := !amp +. amp' *. dt;
      return ans
    in
    let set s =
      (* Printf.printf "new state: %s\n%!" (match s with `Attack -> "a" | `Decay -> "d" | `Sustain -> "s" | `Release -> "r" | `Dead -> "x"); *)
      state := s
    in
    let rec stream ?(a=0.01) ?(d=0.05) ?(s=0.8) ?(r=0.5) ?(sustain=true) ?(release=`Linear) () =
      let* _ = dt in
      match !state with
      | `Dead -> return 0.
      | `Sustain -> return s
      | `Release ->
        (
          match release with
          | `Linear ->
            let* a = integ (-. s /. r) in
            if a <= 0.0001 || s <= 0.001 || r <= 0.001 then die () else return a
          | `Exponential ->
            let a = integ (-. log2 /. r *. !amp) in
            if !amp <= 0.001 || r <= 0.001 then die () else a
        )
      | `Decay -> if !amp <= s || d <= 0.0001 then (amp := s; set (if sustain then `Sustain else `Release); stream ()) else integ ((s -. 1.) /. d)
      | `Attack ->
        if !amp >= 1. || a <= 0.0001 then (set `Decay; stream ()) else integ (1. /. a)
    in
    let handler = function
      | `Release -> set `Release
      | `Reset -> amp := 0.; set `Attack
    in
    Event.register event handler;
    stream

  (** Exponential decay with given parameter. *)
  let exponential ?(init=1.) () =
    let y = ref init in
    fun k ->
      let* dt = dt in
      let ans = !y in
      y := !y *. (1. +. k *. dt);
      return ans

  (** Same as above but taking half-life as parameter. *)
  let exponential_hl ?init () =
    let ln2 = log 2. in
    let e = exponential ?init () in
    fun h -> e (-. ln2 /. h)

  (** Affine from a value to a value in a given time. *)
  let ramp ?(kind=`Linear) () =
    match kind with
    | `Linear ->
      let arrived = ref false in
      let t = integrate ~periodic:true ~on_reset:(fun () -> arrived := true) () in
      fun ?(from=0.) ?(target=1.) duration ->
        let* _ = dt in
        let a = target -. from in
        let a' = 1. /. duration in
        if duration = 0. then arrived := true;
        stream_ref arrived >>=
        switch
          (return target)
          (let* t = t a' in return (a *. t +. from))
    | `Exponential ->
      let e = exponential_hl () in
      fun ?(from=0.) ?(target=1.) duration ->
        let* e = e duration in
        return ((1. -. e) *. (target -. from) +. from)
end

let adsr = Envelope.adsr

(** Smoothen the stream. This is useful to avoid big jumps in controllers. The
    parmeter is roughly the time taken to reach the desired value. *)
let smooth ?(init=0.) ?(kind=`Exponential) () =
  let x = ref init in
  match kind with
  | `Exponential ->
    fun a target ->
      let* dt = dt in
      x := !x +. (dt /. a) *. (target -. !x);
      return !x
  | `Linear ->
    fun a target ->
      let* dt = dt in
      if !x = target then ()
      else if !x < target then
        (
          x := !x +. a *. dt;
          if !x > target then x := target
        )
      else
        (
          x := !x -. a *. dt;
          if !x < target then x := target
        );
      return !x

(** {2 Effects} *)

(** Filters. *)
module Filter = struct
  (** First order filter. *)
  let first_order ?(variant=`Simple) () =
    match variant with
    (* This is the first implementation anyone would write. *)
    | `Simple ->
      (
        (* Previous value for low-pass. *)
        let lp' = ref 0. in
        fun kind freq ->
          let omega = 2. *. Float.pi *. freq in
          fun (x : sample) ->
            let* dt = dt in
            let omega = omega *. dt in
            (* The digital sampling rate... *)
            let omega = omega /. (1. +. omega) in
            let hp = x -. !lp' in
            let lp = !lp' +. hp *. omega /. (1. +. omega) in
            lp' := lp;
            match kind with
            | `Low_pass -> return lp
            | `High_pass -> return hp
      )
    (* The trapezoidal variant is supposed to behave much better at high
       frequencies, see "The art of VA filter design", although this is not what
       I hear... *)
    | `Trapezoidal ->
      (* TODO: we could merge with the above, the only difference is the
         pre-wrapping... *)
      (
        let integrate = integrate ~kind:`Trapezoidal () in
        let lp' = ref 0. in
        fun kind freq ->
          let omega = 2. *. Float.pi *. freq in
          fun x ->
            let* dt = dt in
            (* Pre-wrapping *)
            let omega = tan (omega *. dt /. 2.) in
            let hp = x -. !lp' in
            let* lp = integrate (hp *. omega) in
            lp' := lp;
            match kind with
            | `Low_pass -> return lp
            | `High_pass -> return hp
      )

  (** Biquadratic / second order filter. *)
  (* See https://webaudio.github.io/Audio-EQ-Cookbook/audio-eq-cookbook.html
     http://www.musicdsp.org/files/Audio-EQ-Cookbook.txt *)
  let biquad () =
    let x'  = ref 0. in
    let x'' = ref 0. in
    let y'  = ref 0. in
    let y'' = ref 0. in
    let advance x y =
      x'' := !x';
      x'  := x;
      y'' := !y';
      y'  := y
    in
    fun kind q freq (x : sample) ->
      assert (q > 0.);
      let* dt = dt in
      let w0 = 2. *. Float.pi *. dt *. freq in
      let cw0 = cos w0 in
      let alpha = sin w0 /. (2. *. q) in
      let a0, a1, a2, b0, b1, b2 =
        match kind with
        | `Low_pass ->
          let ocw0 = 1. -. cw0 in
          let ocw02 = ocw0 /. 2. in
          1. +. alpha, (-2.) *. cw0, 1. -. alpha,
          ocw02, ocw0, ocw02
        | `High_pass ->
          let ocw0 = 1. +. cw0 in
          let ocw02 = ocw0 /. 2. in
          1. +. alpha, (-2.) *. cw0, 1. -. alpha,
          ocw02, -.ocw0, ocw02
        | `Band_pass ->
          1.+.alpha, -.2.*.cw0, 1.-.alpha,
          q*.alpha, 0., -.q*.alpha
        | `Notch ->
          let a1 = -.2.*.cw0 in
          1.+.alpha, a1, 1.-.alpha,
          1., a1, 1.
        | `All_pass ->
          let a0 = 1.+.alpha in
          let a1 = -.2.*.cw0 in
          let a2 = 1.-.alpha in
          a0, a1, a2,
          a2, a1, a0
      in
      let y = (b0 *. x +. b1 *. !x' +. b2 *. !x'' -. a1 *. !y' -. a2 *. !y'') /. a0 in
      advance x y;
      return y

  (** Moog-type n pole ladder filter. *)
  let ladder ?(order=4) () =
    let fo = List.init order (fun _ -> first_order ()) in
    let y = ref 0. in (* previous output *)
    fun kind q freq x ->
      let* _ = dt in
      let x = x -. q *. !y in
      let* x = List.fold_left (fun x fo -> x >>= fo kind freq) (return x) fo in
      y := x;
      return x      
end

module Ringbuffer = struct
  type t = Sample.Ringbuffer.t

  let create () = Sample.Ringbuffer.create ()

  let past r delay =
    let* delay = samples delay in
    return (Sample.Ringbuffer.past r delay)

  let write r x = Sample.Ringbuffer.write r x

  let prepare r len =
    let* len = samples len in
    return (Sample.Ringbuffer.prepare r len)

  (* let advance (dt,r) = Sample.Ringbuffer.advance r *)
end

(** Delay effect on the stream. *)
let delay () =
  let r = Ringbuffer.create () in
  fun ?(dry=1.) ?(wet=0.5) ?(feedback=0.9) delay x ->
    let* () = Ringbuffer.prepare r delay in
    let* x' = Ringbuffer.past r delay in
    let ans = dry *. x +. wet *. x' in
    Ringbuffer.write r (x +. feedback *. ans);
    return ans

(** A {{: https://en.wikipedia.org/wiki/Comb_filter} comb filter}. *)
let comb ?kind () =
  let comb = Sample.comb ?kind () in
  fun delay a x ->
    let* delay = samples delay in
    comb delay a x

(** A {{: https://ccrma.stanford.edu/~jos/Delay/Schroeder_Allpass_Filters.html }
   Schroeder allpass filter}. *)
let schroeder_allpass () =
  let sa = Sample.schroeder_allpass () in
  fun delay ->
    let* delay = samples delay in
    sa delay

(** A simple delay with no dry or feedback. *)
let simple_delay () =
  let d = Sample.delay () in
  fun delay x ->
    let* delay = samples delay in
    d delay x

(** Auto gain control. *)
let agc ?(period=0.1) ?(up=0.5) ?(down=15.) ?(blank=0.01) ?(target=0.8) ?(clipping=true) () =
  let target = target *. target in
  let blank = blank *. blank in
  let up = up *. period in
  let down = down *. period in
  let n = ref 0 in
  let ss = ref 0. in
  let clipped = ref false in
  let a = ref 1. in
  fun x ->
    let ans = !a *. x in
    let ans2 = ans *. ans in
    ss := !ss +. ans2;
    if ans2 > 1. then clipped := true;
    incr n;
    let* period = samples period in
    if !n >= period then
      (
        let ms = !ss /. float period in
        if ms > blank then
          if ms <= target then
            if clipping && !clipped then
              a := !a -. (target -. ms) *. down
            else
              a := !a +. (target -. ms) *. up
          else
            a := !a +. (target -. ms) *. down;
        n := 0;
        ss := 0.;
        clipped := false;
      (* Printf.printf "amp: %f\n%!" !a *)
      );
    return ans

(** Slicers: those regularly mute the stream according to various patterns. *)
module Slicer = struct
  let hachoir () =
    let p = periodic () in
    fun duration ?(width=0.5) x ->
      let* y = p (1. /. duration) in
      if y <= width then return x else return 0.

  let staccato () =
    let event = Event.create () in
    let adsr = adsr ~event () in
    let lpf = Filter.biquad () `Low_pass in
    let reset () = Event.emit event `Reset in
    let every = every () in
    fun ?a ?d ?s ?(lp=true) ?(lp_q=1.) ?(lp_freq=10000.) time x ->
      let* () = every time >>= on reset in
      let* a = adsr ?a ?d ?s () in
      if lp then
        lpf lp_q (lp_freq *. a) (x *. a)
      else
        return (x *. a)

  let eurotrance () =
    let p = periodic () in
    fun duration x ->
      let* t = p (1. /. duration) in
      let d = int_of_float (t *. 8.) in
      if d = 0 || d = 4 || d = 6 then return x else return 0.
end

(** Chorus effect. *)
let chorus () =
  let d = simple_delay () in
  fun ?(wet=1.) delay x ->
    let delay = max 0. delay in
    let* x' = d delay x in
    return (x +. wet *. x')

(** Flanger effect. *)
(* TODO: add optional feedback *)
let flanger () =
  let chorus = chorus () in
  let lfo = triangle () in
  fun delay ?(wet=1.) freq x ->
    let* t = lfo freq in
    let delay = delay /. 2. *. (1. +. t) in
    chorus ~wet delay x

(** Distortion effects. *)
module Distortion = struct
  (* amount in [-1,1] *)
  let waveshaper () =
  fun amount x ->
    let k = 2. *. amount /. (1. -. amount) in
    return ((1. +. k) /. (1. +. k *. abs_float x))

  let convolver amount =
    let len = 128 in
    let a = Array.init len (fun i -> if i = 0 then 1. else (Random.float (2. *. float (len - i) /. float len) -. 1.) *. amount) in
    Sample.convolve a
end

(** {2 Analysis} *)

(** Print value of stream. *)
let print ?first ?(changes=true) name =
  if changes then on_change ?first (fun x -> Printf.printf "%s: %f\n%!" name x)
  else (fun x -> Printf.printf "%s: %f\n%!" name x; return x)

(** Blink a led on tempo. *)
let blink_tempo on off =
  let p = periodic () in
  let au = activates () in
  let ad = activates () in
  fun ?(duration=0.25) tempo ->
    let* t = p (tempo /. 60.) in
    let* up = au (t < duration) in
    let* down = ad (t >= duration) in
    if up then return (on ())
    else if down then return (off ())
    else return ()

(** Power (mean square) of a stream. *)
let ms duration cb =
  let sq = ref 0. in
  let n = ref 0 in
  fun x ->
    let* samples = samples duration in
    let fsamples = float_of_int samples in
    sq := !sq +. x *. x;
    incr n;
    if !n >= samples then
      (
        n := 0;
        cb (!sq /. fsamples);
        sq := 0.
      );
    return ()

(** Is a stream blank? *)
let is_blank duration =
  let t = ref 0. in
  let ans = ref false in
  let cb ms =
    let t = !t in
    ans := ms <= t *. t
  in
  let ms = ms duration cb in
  fun threshold ->
    t := threshold;
    fun x -> ms x >> stream_ref ans

(** {2 Stereo streams} *)

(** Operations on stereo streams. *)
module Stereo = struct
  (** A stereo stream. *)
  type 'a t = ('a * 'a) stream

  (** Create from mono stream. *)
  let of_mono x : 'a t =
    return (x, x)

  (** Blank stereo stream. *)
  let blank = of_mono 0.

  (** Construct a stereo stream from two mono streams. *)
  let merge s1 s2 =
    let* x = s1 in
    let* y = s2 in
    return (x, y)

  let noise () =
    merge (noise ()) (noise ())

  (** Add two stereo streams. *)
  let add s1 s2 =
    let* x1,y1 = s1 in
    let* x2,y2 = s2 in
    return (x1 +. x2, y1 +. y2)

  (** Delay. *)
  let delay ?ping_pong () =
    let delay_l = delay () in
    let delay_r = delay () in
    (* Initial additional delay. *)
    let delay_r0 =
      match ping_pong with
      | Some ping_pong -> simple_delay () (ping_pong /. 2.)
      | None -> return
    in
    fun ?dry ?wet ?(feedback=0.6) delay ((x,y) as c) ->
      let x = delay_l ?dry ?wet ~feedback delay x in
      let y = delay_r0 y >>= delay_r ?dry ?wet ~feedback delay in
      add (return c) (merge (bind (mul feedback) x) (bind (mul feedback) y))

  (** Mix streams. *)
  let mix ss =
    List.fold_left add blank ss

  (** Amplify. *)
  let amp a (x,y) = return (a *. x, a *. y)

  (** Multiply by a constant. *)
  let cmul a s =
    s >>= amp a

  (** Multiply by a boolean. *)
  let bmul b s =
    bind2 (fun b c -> if b then return c else return (0.,0.)) b s

  (** Map functions. *)
  let map fl fr (x,y) =
    let* x = fl x in
    let* y = fr y in
    return (x, y)

  (** Convert to mono. *)
  let to_mono (x,y) =
    return ((x +. y) /. 2.)

  (** Left channel. *)
  let left (x,_) = return x

  (** Right channel. *)
  let right (_,y) = return y

  (** Dephase channels by given delay. *)
  let dephase () =
    let delay_l = simple_delay () in
    let delay_r = simple_delay () in
    fun delay ->
      let dl, dr = if delay < 0. then -.delay, 0. else 0., delay in
      fun (x,y) ->
        let* x = delay_l dl x in
        let* y = delay_r dr y in
        return (x, y)

  (** Pan the sound according to a number between -1 (full left) and 1 (full
     right). Various {{:
     http://www.cs.cmu.edu/~music/icm-online/readings/panlaws/} pan laws} can be
     used. *)
  let pan ?(law=`Circular) =
    fun a ->
    let a = (a +. 1.) /. 2. in
    let l, r =
      match law with
      | `Linear ->
        1. -. a, a
      | `Circular -> (* Equal power *)
        cos (a *. Float.pi /. 2.),
        sin (a *. Float.pi /. 2.)
      | `Mixed ->
        (* The -4.5dB pan law *)
        sqrt ((1. -. a) *. cos (a *. Float.pi /. 2.)),
        sqrt ((1. -. a) *. sin (a *. Float.pi /. 2.))
    in
    fun (x : sample) -> (return (l *. x, r *. x) : sample t)

  (** {{: https://ccrma.stanford.edu/~jos/pasp/Schroeder_Reverberators.html} Schroeder reverberation}. *)
  let schroeder ?(size=`Small) () =
    let kind = match size with `Small -> `Feedforward | `Large -> `Feedback in
    let fbcf d g = comb ~kind () d (-.g) in
    let ap () = schroeder_allpass () in
    (* Original values are given for a 25 kHz sampling rate .*)
    let ap1 = ap () (347./.25000.) 0.7 in
    let ap2 = ap () (113./.25000.) 0.7 in
    let ap3 = ap () (37./.25000.) 0.7 in
    let fbcf1 = fbcf (1687./.25000.) 0.773 in
    let fbcf2 = fbcf (1601./.25000.) 0.802 in
    let fbcf3 = fbcf (2053./.25000.) 0.753 in
    let fbcf4 = fbcf (2251./.25000.) 0.733 in
    fun x ->
      let* i = ap1 x >>= ap2 >>= ap3 in
      let* x1 = fbcf1 i in
      let* x2 = fbcf2 i in
      let* x3 = fbcf3 i in
      let* x4 = fbcf4 i in
      return (x1+.x3, x2+.x4)

  let schroeder_random () =
    (* Feedback comb-filter. *)
    let fbcf d g = comb () d (-.g) in
    let ap = schroeder_allpass () in
    let ap1 = ap (Random.float 0.015 +. 0.001) 0.7 in
    let ap2 = ap (Random.float 0.015 +. 0.001) 0.7 in
    let ap3 = ap (Random.float 0.015 +. 0.001) 0.7 in
    let fbcf1 = fbcf (Random.float 0.05 +. 0.05) 0.773 in
    let fbcf2 = fbcf (Random.float 0.05 +. 0.05) 0.802 in
    let fbcf3 = fbcf (Random.float 0.05 +. 0.05) 0.753 in
    let fbcf4 = fbcf (Random.float 0.05 +. 0.05) 0.733 in
    fun x ->
      let* i = ap1 x >>= ap2 >>= ap3 in
      let* x1 = fbcf1 i in
      let* x2 = fbcf2 i in
      let* x3 = fbcf3 i in
      let* x4 = fbcf4 i in
      return (x1+.x3, x2+.x4)

  let schroeder2 () =
    let fbcf d g = comb () d (-.g) in
    let ap () = schroeder_allpass () in
    let fbcf1 = fbcf (901./.25000.) 0.805 in
    let fbcf2 = fbcf (778./.25000.) 0.827 in
    let fbcf3 = fbcf (1011./.25000.) 0.783 in
    let fbcf4 = fbcf (1123./.25000.) 0.764 in
    let ap1 = ap () (125./.25000.) 0.7 in
    let ap2 = ap () (42./.25000.) 0.7 in
    let ap3 = ap () (12./.25000.) 0.7 in
    let add4 x1 x2 x3 x4 = return (x1+.x2+.x3+.x4) in
    fun x ->
      bind4 add4 (fbcf1 x) (fbcf2 x) (fbcf3 x) (fbcf4 x) >>= ap1 >>= ap2 >>= ap3 >>= (fun x -> return (x, -.x))

  (** Automatic gain control. *)
  let agc () =
    let agcl = agc () in
    let agcr = agc () in
    map agcl agcr

  module Envelope = struct
    let apply e =
      fun (x, y) ->
        let* e = e in
        return (e *. x, e *. y)
  end

  module Slicer = struct
    let eurotrance () =
      let l = Slicer.eurotrance () in
      let r = Slicer.eurotrance () in
      fun duration ->
        map (l duration) (r duration)
  end

  (** {2 Effects} *)

  (** The {{: https://ccrma.stanford.edu/~jos/pasp/Freeverb.html } freeverb} reverberation. *)
  (* TODO: this assumes 44.1 kHz sampling rate *)
  let freeverb () =
    (* Constants. *)
    let wet_scale = 3. in
    let dry_scale = 2. in
    let damp_scale = 0.4 in
    let room_scale = 0.28 in
    let room_offset = 0.7 in
    (* Filter parameters. *)
    let damp1 = ref 0.5 in
    let damp2 = ref 0.5 in
    let comb_feedback = ref (0.5 *. 0.28 +. 0.7) in
    let stereo_spread = 23 in
    let combl = [1116; 1188; 1277; 1356; 1422; 1491; 1557; 1617] in
    let combr = List.map (fun n -> n + stereo_spread) combl in
    let apl = [556; 441; 341; 225] in
    let apr = List.map (fun n -> n + stereo_spread) apl in
    let ap_feedback = 0.5 in
    let gain = 0.015 in
    (* Comb filter. *)
    let comb len =
      let buf = Array.make len 0. in
      let pos = ref 0 in
      let filterstore = ref 0. in
      fun input ->
        let* _ = dt in
        let output = buf.(!pos) in
        filterstore := output *. !damp2 +. !filterstore *. !damp1;
        buf.(!pos) <- input +. !filterstore *. !comb_feedback;
        incr pos;
        if !pos = len then pos := 0;
        return output
    in
    (* All-pass filter. *)
    let ap len =
      let buf = Array.make len 0. in
      let pos = ref 0 in
      fun x ->
        let y' = buf.(!pos) in
        let o = y' -. x in
        buf.(!pos) <- x +. (ap_feedback *. y');
        incr pos;
        if !pos = len then pos := 0;
        return o
    in
    let combl = List.map comb combl in
    let combr = List.map comb combr in
    let apl = List.map ap apl in
    let apr = List.map ap apr in
    let apl = List.compose (List.map bind apl) in
    let apr = List.compose (List.map bind apr) in
    fun  ?(roomsize=0.5) ?(damp=0.5) ?(width=1.) ?(wet=1./.3.) ?(dry=0.) ->
      (* Update parameters. *)
      let roomsize = roomsize *. room_scale +. room_offset in
      let damp = damp *. damp_scale in
      let wet = wet *. wet_scale in
      let dry = dry *. dry_scale in
      let wet1 = wet *. (width /. 2. +. 0.5) in
      let wet2 = wet *. ((1. -. width) /. 2.) in
      damp1 := damp;
      damp2 := 1. -. damp;
      comb_feedback := roomsize;
      fun (x,y) ->
        (* Apply filters. *)
        let i = (x +. y) *. gain in
        let combl = List.map (fun c -> c i) combl in
        let combr = List.map (fun c -> c i) combr in
        let outl = List.fold_left (funct2 (+.)) (return 0.) combl in
        let outr = List.fold_left (funct2 (+.)) (return 0.) combr in
        let* outl = apl outl in
        let* outr = apr outr in
        let x = outl *. wet1 +. outr *. wet2 +. x *. dry in
        let y = outr *. wet1 +. outl *. wet2 +. y *. dry in
        return (x,y)

  (** Testing reverb with convolution with noise. *)
  let converb ?(duration=1.) () =
    (* Mono *)
    let cv () =
      let len = int_of_float (44100. *. duration) in
      let a = Array.init len (fun i -> Random.float (float i /. float len)) in
      Sample.convolve a
    in
    let l = cv () in
    let r = cv () in
    map l r
end

(** Duplicate a mono stream to become a stereo stream. *)
let stereo = Stereo.of_mono

(** Binded functions: those operate on streams instead of samples. *)
module B = struct
  (** Add a constant. *)
  let cadd x = bind (add x)

  (** Add two streams. *)
  let add = bind2 add

  (** Multiply by a constant. *)
  let cmul x = bind (mul x)

  (** Multiply by a constant. *)
  let mulc x y = cmul y x

  (** Multiply two streams. *)
  let mul = bind2 mul

  (** Multiply by a boolean. *)
  let bmul = bind2 bmul

  let mix = bind_list mix
end
