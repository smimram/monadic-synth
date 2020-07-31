open Extlib

type sample = float

(** {2 Stream monad} *)

type 'a t = unit -> 'a

type 'a stream = 'a t

let return : 'a -> 'a t = fun x () -> x

let bind : ('a -> 'b t) -> 'a t -> 'b t =
  fun f x () -> f (x ()) ()

let apply : ('a -> 'b) t -> 'a t -> 'b t =
  fun f x () -> f () (x ())

let bind2 f x y =
  bind (fun x -> bind (f x) y) x

let bind3 f x y z =
  bind (fun x -> bind (fun y -> bind (f x y) z) y) x

let bind4 f x y z t =
  bind (fun x -> bind (fun y -> bind (fun z -> bind (f x y z) t) z) y) x

let bind5 f x y z t u =
  bind (fun x -> bind (fun y -> bind (fun z -> bind (fun t -> bind (f x y z t) u) t) z) y) x

let bind6 f x y z t u v =
  bind (fun x -> bind (fun y -> bind (fun z -> bind (fun t -> bind (fun u -> bind (f x y z t u) v) u) t) z) y) x

let funct : ('a -> 'b) -> 'a t -> 'b t =
  fun f x -> bind (fun x -> return (f x)) x

let funct2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t =
  fun f x y -> bind2 (fun x y -> return (f x y)) x y

let prod : 'a t -> 'b t -> ('a * 'b) t =
  fun x y () -> (x (), y ())

let seq : (unit -> 'a) -> 'a stream =
  fun f -> f

let get : 'a stream -> 'a =
  fun f -> f ()

(* Nice explanation of monadic syntax at https://jobjo.github.io/2019/04/24/ocaml-has-some-new-shiny-syntax.html *)
module Common = struct
  let return = return

  let ( >>= ) x f = bind f x

  let ( >> ) x f = x >>= (fun () -> f)

  let ( <$> ) = funct

  let ( <*> ) = apply

  let ( let* ) x f = bind f x

  let ( let+ ) x f = funct x f

  let ( and+ ) = prod
end

include Common

(** {2 Pure operations} *)

let print ?(every=1) name =
  let n = ref 0 in
  fun x ->
    incr n;
    if !n >= every then
      (
        Printf.printf "%s: %f\n%!" name x;
        n := 0;
      );
    return x

(** Forget the result of the stream. *)
let drop _ = return ()

(** Previous value of the stream. *)
let prev (x0:'a) : 'a -> 'a t =
  let prev = ref x0 in
  fun x ->
    let ans = !prev in
    prev := x;
    return ans

(** Stream duplication. Once the left part has been evaluated, the right part
    can be used as many times as wanted. This has to be used if you need to use a
    stream more than once, in order to avoid each copy asking for a different
    sample. *)
let dup () : 'a t -> unit t * 'a t =
  let x = ref None in
  fun s ->
    s >>= (fun y -> return (x := Some y)),
    fun () -> try Option.get !x with _ -> failwith "Invalid evaluation order in dup."

let stream_ref : 'a ref -> 'a t =
  fun x () -> !x

let samples ~dt =
  fun t -> round (t /. dt)

(** Sparse stream monad *)
module Sparse = struct
  type 'a t = ('a -> unit) -> unit

  let return : 'a -> 'a t =
    fun x k -> k x

  let bind : ('a -> 'b t) -> 'a t -> 'b t =
    fun f x k -> x (fun x -> f x k)

  let to_stream init (s : 'a t) =
    let x = ref init in
    let f x' = x := x' in
    s f;
    stream_ref x
end

(** {2 Arithmetic} *)

let cst x : sample t = return x

let blank = cst 0.

let cmul a s =
  let f x = return (a *. x) in
  s >>= f

let bmul b s =
  bind2 (fun b x -> if b then return x else return 0.) b s

let cadd a s =
  let f x = return (a +. x) in
  s >>= f

let mul = funct2 ( *. )

let amp x y = return (x *. y)

let add = funct2 ( +. )

let rec add_list ss =
  List.fold_left (funct2 (+.)) blank ss

let sub = funct2 ( -. )

let clip x = return (max (-1.) (min 1. x))

let soft_clip x =
  if x <= -1. then (-2.)/.3.
  else if x >= 1. then 2./.3.
  else x-.x*.x*.x/.3.

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

(** Event hubs. *)
module Event = struct
  type 'a t = ('a -> unit) list ref

  let create () : 'a t = ref []

  let register (h:'a t) f =
    h := f :: !h

  let emit (h:'a t) e =
    List.iter (fun f -> f e) !h

  let merge h1 h2 = ref (!h1 @ !h2)

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

(** Operations with samples as unit time. *)
module Sample = struct
  let convolve a =
    let len = Array.length a in
    let prev = Array.make len 0. in
    let n = ref 0 in
    fun x () ->
      prev.(!n) <- x;
      let ans = ref 0. in
      for i = 0 to len - 1 do
        ans := !ans +. a.(i) *. prev.((!n - i + len) mod len);
      done;
      incr n;
      if !n = len then n := 0;
      !ans

  let ringbuffer maxduration =
    let buflen = maxduration + 1 in
    let buf = Array.make buflen 0. in
    let pos = ref 0 in
    let read delay : sample t =
      assert (0 <= delay && delay <= maxduration);
      let prev = !pos - delay in
      let prev = if prev < 0 then prev + buflen else prev in
      return buf.(prev)
    in
    let write x =
      buf.(!pos) <- x
    in
    let advance () =
      incr pos;
      if !pos >= buflen then pos := 0
    in
    read, write, advance

  let delay maxdelay =
    let peek, write, advance = ringbuffer maxdelay in
    fun ?(delay=maxdelay) x ->
      write x; peek delay >>= (fun x -> advance (); return x)

  (** A fixed delay which inputs the sample at t-delay, passes it to a function,
      and writes the result. Useful for implementing the recursive part of
      filters. *)
  let rec_delay maxdelay =
    let read, write, advance = ringbuffer maxdelay in
    fun ?(delay=maxdelay) f ->
      read delay >>= f >>= (fun y -> write y; advance (); return y)

  (** Feedback comb filter. *)
  let comb m =
    let d = delay m in
    fun a x ->
      d x >>= (fun x' -> return (x-.a*.x'))

  (** All-pass filter. *)
  let schroeder_allpass m =
    let dx = delay m in
    let dy = rec_delay m in
    fun g x ->
      dy (fun y' -> dx x >>= (fun x' -> return (x'+.g*.(y'-.x))))

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
      let theta = (-2.) *. pi *. float k /. float n in
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

let integrate ~dt ?(event=Event.create ()) ?(on_reset=nop) ?(init=0.) ?(periodic=false) () : float -> sample t =
  let y = ref init in
  let handler = function
    | `Reset -> y := init; on_reset ()
    | `Set x -> y := x
  in
  Event.register event handler;
  fun x () ->
    let ans = !y in
    y := !y +. x *. dt;
    if !y >= 1. && periodic then (y := !y -. 1.; on_reset ());
    ans

let now ~dt ?event () : sample t =
  integrate ~dt ?event () 1.

(* TODO: implement periodic with events *)
let periodic ~dt ?(init=0.) ?on_reset () =
  integrate ~periodic:true ~dt ~init ?on_reset ()

let exponential ?(init=1.) ~dt =
  let y = ref 1. in
  fun k () ->
    let ans = !y in
    y := !y *. (1. +. k *. dt);
    ans

(** Same as above but taking half-life as parameter. *)
let exponential_hl ?init ~dt =
  let e = exponential ?init ~dt in
  fun h -> e (-. log 2. /. h)

let saw ~dt : float -> sample t =
  let p = periodic ~dt () in
  fun freq ->
    p freq >>= (fun x -> return (2. *. x -. 1.))

let triangle ~dt =
  let p = periodic ~dt ~init:0.25 () in
  let f t =
    if t <= 0.5 then
      return (4. *. t -. 1.)
    else
      return (3. -. 4. *. t)
  in
  fun freq -> p freq >>= f

let sine ~dt : float -> sample t =
  let p = periodic ~dt () in
  let a = 2. *. pi in
  fun freq ->
    p freq >>= (fun t -> return (sin (a *. t)))

let square ~dt =
  let square (w:float) (x:float) : sample t = return (if x <= w then -1. else 1.) in
  let p = periodic ~dt () in
  fun ?(width=0.5) freq ->
    p freq >>= square width

let noise : sample t =
  fun () -> Random.float 2. -. 1.

let sampler ~dt ?(interpolation=`Closest) ?(freq=1.) buf =
  let p = periodic ~dt () in
  let buflen = Array.length buf in
  let fbuflen = float buflen in
  let f =
    match interpolation with
    | `Closest ->
      (* TODO: this is actually not the closest but the one below *)
      (fun t ->
        let n = int_of_float (fbuflen *. t) in
        return buf.(n))
  in
  fun freq' ->
    p (freq'/.(freq*.dt*.fbuflen)) >>= f

module Spectral = struct
  module Window = struct
    let hamming buf =
      let n = float (Array.length buf) in
      let f k = 0.54 -. 0.46 *. cos (2.*.pi*.float k/.(n-.1.)) in
      Array.mapi (fun k x -> Complex.cmul (f k) x) buf
  end

  let sampler ~dt ?freq ?interpolation buf =
    let buf = Array.map Complex.re (Sample.ifft buf) in
    sampler ~dt ?interpolation ?freq buf

  (* See http://zynaddsubfx.sourceforge.net/doc/PADsynth/PADsynth.htm *)
  let pad ~dt ?(bandwidth=40.) ?(harmonics=Array.init 64 (fun i -> 1./.(float i)*.(if i mod 2 = 0 then 2. else 1.))) () =
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
    sampler ~dt ~freq:f0 spectrum

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
    sampler ~dt ~freq:f0 buf
end

(** {2 Control} *)

(** When a stream becomes true. *)
let activated () =
  let prev = ref false in
  fun b ->
    let p = !prev in
    prev := b;
    return (not p && b)

let changed () =
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
  let activated = activated () in
  fun s ->
    s >>= (fun x -> return (x >= 0.)) >>= activated

(** Check whether we are at a particular instant. *)
let at ~dt =
  let now = now ~dt in
  let activated = activated () in
  fun time -> now >>= (fun t -> return (t >= time)) >>= activated

(** Generate an event every period of time. *)
let every ~dt =
  let b = ref false in
  let on_reset () = b := true in
  let p = periodic ~dt ~on_reset () in
  let ans () = if !b then (b := false; return true) else return false in
  fun time ->
    let freq = 1. /. time in
    p freq >>= drop >> ans ()

(** Execute an action when a stream is true. *)
let on f b =
  if b then return (f ()) else return ()

(** Execute a function when a stream change its value. *)
let on_change f =
  let old = ref None in
  fun x ->
    match !old with
    | Some x0 when x0 = x -> return x
    | Some _ -> old := Some x; f x; return x
    | None -> old := Some x; return x

(** Print value of stream when it changes. *)
let print name =
  on_change (fun x -> Printf.printf "%s: %f\n%!" name x)

let fallback (x:'a t) (y:'a t) b : 'a t =
  if b then x else y

let fallblank x b = fallback x blank b

(** {2 Oscillators} *)

(** Generate a random value at given frequency. *)
let random ~dt =
  let x = ref 0. in
  let every = every ~dt in
  fun ?(min=0.) ?(max=1.) freq ->
    every (1. /. freq) >>= on (fun () -> x := Random.float (max -. min) +. min) >> stream_ref x

(** Generic oscillator. *)
let osc ~dt kind =
  match kind with
  | `Sine -> sine ~dt
  | `Square -> square ~dt ?width:None
  | `Saw -> saw ~dt
  | `Triangle -> triangle ~dt
  | `Noise -> (fun freq -> noise)
  | `Random -> random ~dt ~min:(-1.) ~max:(1.)

(** Frequency modulation synthesis. *)
let fm ~dt ?(carrier=`Sine) ?(modulator=`Sine) () =
  let carrier = osc ~dt carrier in
  let modulator = osc ~dt modulator in
  fun ?(ratio=1.) depth freq ->
    modulator (ratio *. freq) >>= (fun m -> carrier (freq +. depth *. m))

let random_zero ~dt =
  let x = ref 0. in
  fun ?(attraction=500.) speed ->
    let attraction = attraction *. dt in
    let speed = speed *. dt in
    fun () ->
      let ans = !x in
      let to_zero = Random.float 1. < attraction in
      let d = (Random.float 2. -. 1.) *. speed in
      let d = if to_zero && (!x *. d) > 0. then -.d else d in
      x := ans +. d;
      ans

let karplus_strong ~dt ?(f=return) freq =
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
  ks >>= f >>= write

(** ADSR envelope *)
let adsr ?(event=Event.create ()) ?(on_die=ignore) ~dt () =
  let state = ref `Attack in
  let log2 = log 2. in
  let amp = ref 0. in
  let die () = state := `Dead; on_die (); amp := 0.; 0. in
  let integ amp' =
    let ans = !amp in
    amp := !amp +. amp' *. dt;
    ans
  in
  let set s =
    (* Printf.printf "new state: %s\n%!" (match s with `Attack -> "a" | `Decay -> "d" | `Sustain -> "s" | `Release -> "r" | `Dead -> "x"); *)
    state := s
  in
  let rec stream ?(a=0.01) ?(d=0.05) ?(s=0.8) ?(r=0.5) ?(sustain=true) ?(release=`Linear) () =
    match !state with
    | `Dead -> 0.
    | `Sustain -> s
    | `Release ->
      (
        match release with
        | `Linear ->
          let a = integ (-. s /. r) in
          if a <= 0.0001 || s <= 0.001 || r <= 0.001 then die () else a
        | `Exponential ->
          if !amp <= 0.001 || r <= 0.001 then die () else integ (-. log2 /. r *. !amp)
      )
    | `Decay -> if !amp <= s || d <= 0.0001 then (amp := s; set (if sustain then `Sustain else `Release); stream ()) else integ ((s -. 1.) /. d)
    | `Attack -> if !amp >= 1. || a <= 0.0001 then (set `Decay; stream ()) else integ (1. /. a)
  in
  let handler = function
    | `Release -> set `Release
    | `Reset -> amp := 0.; set `Attack
  in
  Event.register event handler;
  stream

let ramp ~dt from =
  let arrived = ref false in
  let changed = changed () in
  let move = integrate ~dt ~init:from () in
  fun dest duration ->
    let a' = (dest-.from)/.duration in
    let move = move a' in
    let move =
      let d,move = dup () move in
      d >> move >>= (fun x -> changed (x <= dest)) >>= on (fun () -> arrived := true) >> move
    in
    stream_ref arrived >>= fallback (return dest) move

(** {2 Effects} *)

module Filter = struct
  let first_order ~dt =
    let x' = ref 0. in
    let y' = ref 0. in
    fun kind freq ->
      let rc = 1. /. (2. *. pi *. freq) in
      fun x ->
        match kind with
        | `Low_pass ->
          (fun () ->
            let a = dt /. (rc +. dt) in
            let y = !y' +. a *. (x -. !y') in
            y' := y;
            y)
        | `High_pass ->
          (fun () ->
            let a = rc /. (rc +. dt) in
            let y = a *. (!y' +. x -. !x') in
            x' := x;
            y' := y;
            y)

  (* See http://www.musicdsp.org/files/Audio-EQ-Cookbook.txt *)
  let biquad ~dt =
    let x' = ref 0. in
    let x'' = ref 0. in
    let y' = ref 0. in
    let y'' = ref 0. in
    let advance x y =
      x'' := !x';
      x' := x;
      y'' := !y';
      y' := y
    in
    let w0 = 2. *. pi *. dt in
    fun kind q freq x : sample t ->
    fun () ->
      assert (q > 0.);
      let w0 = w0 *. freq in
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
      y
end

let ringbuffer ~dt maxduration =
  let peek, write, advance = Sample.ringbuffer (samples ~dt maxduration) in
  let peek delay = peek (samples ~dt delay) in
  peek, write, advance

let delay ~dt maxdelay =
  let peek, write, advance = ringbuffer ~dt maxdelay in
  fun ?(dry=1.) ?(wet=0.5) ?(feedback=0.9) ?(delay=maxdelay) x ->
    let f x xd =
      let ans = dry *. x +. wet *. xd in
      write (x +. feedback *. ans);
      advance ();
      return ans
    in
    peek delay >>= (f x)

let comb ~dt delay =
  Sample.comb (samples ~dt delay)

let schroeder_allpass ~dt delay =
  Sample.schroeder_allpass (samples ~dt delay)

(** A simple delay with no dry or feedback. *)
let simple_delay ~dt maxdelay =
  let d = Sample.delay (samples ~dt maxdelay) in
  fun ?(delay=maxdelay) -> d ~delay:(samples ~dt delay)

(** Auto gain control. *)
let agc ~dt ?(period=0.1) ?(up=0.5) ?(down=15.) ?(blank=0.01) ?(target=0.8) ?(clipping=true) () =
  let target = target *. target in
  let blank = blank *. blank in
  let up = up *. period in
  let down = down *. period in
  let period = samples ~dt period in
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

module Slicer = struct
  let hachoir ~dt =
    let p = periodic ~dt () in
    fun duration ?(width=0.5) x ->
      p (1. /. duration) >>= (fun y -> if y <= width then return x else return 0.)

  let staccato ~dt ?a ?d ?s ?(lp=true) () =
    let event = Event.create () in
    let adsr = adsr ~dt ~event ?a ?d ?s () in
    let with_lp = lp in
    let lp = Filter.biquad ~dt `Low_pass in
    let reset () = Event.emit event `Reset in
    let every = every ~dt in
    let dup = dup () in
    let f =
      if with_lp then
        fun ~lp_freq ~lp_q x ->
          let dup,adsr = dup adsr in
          dup >> bind2 (lp lp_q) (cmul lp_freq adsr) (cmul x adsr)
      else
        fun ~lp_freq ~lp_q x -> cmul x adsr
    in
    fun ?(lp_q=1.) ?(lp_freq=10000.) time x ->
      every time >>= on reset >> f ~lp_freq ~lp_q x

  let eurotrance ~dt =
    let p = periodic ~dt () in
    fun duration x ->
      p (1./.duration) >>= (fun t ->
        let d = int_of_float (t *. 8.) in
        if d = 0 || d = 4 || d = 6 then return x else return 0.)
end

let chorus ~dt maxdelay =
  let d = simple_delay ~dt maxdelay in
  fun ?(wet=1.) ?(delay=maxdelay) x ->
    d ~delay x >>= (fun x' -> return (x +. wet *. x'))

let flanger ~dt maxdelay =
  let chorus = chorus ~dt maxdelay in
  let sine = sine ~dt in
  let a = maxdelay/.2. in
  fun ?(wet=1.) freq x ->
    cadd a (cmul a (sine freq)) >>= (fun d -> chorus ~wet ~delay:d x)

module Distortion = struct
  (* amount in [-1,1] *)
  let waveshapper ~dt =
  fun amount x ->
    let k = 2. *. amount /. (1. -. amount) in
    return ((1. +. k) /. (1. +. k *. abs_float x))

  let convolver ~dt amount : sample -> sample t =
    let len = 128 in
    let a = Array.init len (fun i -> if i = 0 then 1. else (Random.float (2. *. float (len - i) /. float len) -. 1.) *. amount) in
    Sample.convolve a
end

(** Notes. *)
module Note = struct
  type 'event t = dt:float -> event:('event Event.t) -> on_die:(unit -> unit) -> sample -> float -> sample stream

  let freq ?(detune=0.) n = 440. *. (2. ** ((float n +. detune -. 69.) /. 12.))

  let duration tempo d =
    60. /. tempo *. d

  let simple f : 'a t =
    fun ~dt ~event ~on_die ->
      let alive = ref true in
      let handler = function
        | `Release -> alive := false; on_die ()
      in
      Event.register event handler;
      let f = f ~dt in
      fun freq vol ->
       bmul (stream_ref alive) (cmul vol (f freq))

  let detune ?(cents=return 7.) ?(wet=return 0.5) (note : 'a t) : 'a t =
    fun ~dt ~event ~on_die ->
      let n = note ~dt ~event ~on_die in
      let nd = note ~dt ~event ~on_die in
      fun freq vol ->
        let cents = get cents in
        let wet = get wet in
        let freqd = freq *. (2. ** (cents /. 1200.)) in
        cmul (1.-.wet/.2.) (add (n freq vol) (cmul wet (nd freqd vol)))

  let add n1 n2 : 'a t =
    fun ~dt ~event ~on_die ->
      let n1 = n1 ~dt ~event ~on_die in
      let n2 = n2 ~dt ~event ~on_die in
      fun freq vol ->
        add (n1 freq vol) (n2 freq vol)

  module Drum = struct
    let kick ~dt ?on_die () =
      let s = cmul 150. (exponential ~dt (-9.)) >>= sine ~dt in
      let env = adsr ~a:0.001 ~d:0.1 ~s:0.9 ~sustain:false ~r:0.8 ?on_die ~dt () in
      mul env s

    let snare ~dt ?on_die ?(a=0.01) ?(d=0.03) ?(s=0.7) ?(r=0.07) ?(lp=80000.) () =
      let env = adsr ~dt ?on_die ~a ~d ~s ~sustain:false ~r ~release:`Exponential () in
      let s = noise in
      let dup, env = dup () env in
      let s = mul env s in
      let s = dup >> bind2 (Filter.first_order ~dt `Low_pass) (cmul lp env) s in
      s

    let crash ~dt ?on_die () =
      let s = noise in
      let env = adsr ~dt ?on_die ~a:0.01 ~d:0.05 ~s:0.8 ~sustain:false ~r:0.5 () in
      mul env s

    let closed_hat ~dt ?on_die () =
      let s = noise in
      let env = adsr ~dt ?on_die ~a:0.001 ~d:0.005 ~s:0.3 ~sustain:false ~r:0.01 () in
      let s = s >>= Filter.first_order ~dt `High_pass 4000. in
      mul env s
  end

  (** Simple note with adsr envelope and volume. *)
  let adsr ~dt ~event ~on_die ?a ?d ?s ?r f freq vol =
    let g = function
      | Some x -> Some (get x)
      | None -> None
    in
    (
      match g a with
      | Some a -> Printf.printf "a : %f\n%!" a
      | None -> Printf.printf "a : none\n%!"
    );
    let env = adsr ~dt ~event ~on_die ?a:(g a) ?d:(g d) ?s:(g s) ?r:(g r) () in
    let s = f ~dt freq in
    let s = mul env s in
    cmul vol s
end

(** {2 Analysis} *)

let ms ~dt duration cb =
  let samples = samples ~dt duration in
  let fsamples = float_of_int samples in
  let sq = ref 0. in
  let n = ref 0 in
  fun x ->
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
let is_blank ~dt duration =
  let t = ref 0. in
  let ans = ref false in
  let cb ms =
    let t = !t in
    ans := ms <= t *. t
  in
  let ms = ms ~dt duration cb in
  fun threshold ->
    t := threshold;
    fun x -> ms x >> stream_ref ans

(** Operations on stereo streams. *)
module Stereo = struct
  let of_mono : 'a -> ('a * 'a) t =
    fun x -> return (x, x)

  let blank = of_mono 0.

  let merge s1 s2 =
    fun () -> s1 (), s2 ()

  let add s1 s2 =
    let f (x1,y1) (x2,y2) =
      return (x1 +. x2, y1 +. y2)
    in
    bind2 f s1 s2

  let delay ?ping_pong ~dt maxdelay =
    let delay_l = delay ~dt maxdelay in
    let delay_r = delay ~dt maxdelay in
    (* Initial additional delay. *)
    let delay_r0 =
      match ping_pong with
      | Some ping_pong -> simple_delay ~dt (ping_pong /. 2.) ?delay:None
      | None -> return
    in
    fun ?dry ?wet ?(feedback=0.6) ?delay ((x,y) as c) ->
      let x = delay_l ?dry ?wet ~feedback ?delay x in
      let y = delay_r0 y >>= delay_r ?dry ?wet ~feedback ?delay in
      add (return c) (merge (cmul feedback x) (cmul feedback y))

  let add_list ss =
    List.fold_left (bind2 (fun (xs,ys) (x,y) -> return (xs+.x,ys+.y))) blank ss

  let amp a (x,y) = return (a *. x, a *. y)

  let cmul a s =
    s >>= amp a

  let bmul b s =
    bind2 (fun b c -> if b then return c else return (0.,0.)) b s

  let map (fl:'a -> 'b t) (fr:'c -> 'd t) (x,y) =
    return (fl x (), fr y ())

  let to_mono (x,y) =
    return ((x +. y) /. 2.)

  let left (x,y) = return x

  let right (x,y) = return y

  let dephase ~dt maxdelay =
    let delay_l = simple_delay ~dt (abs_float maxdelay) in
    let delay_r = simple_delay ~dt (abs_float maxdelay) in
    fun ?(delay=maxdelay) (x,y) ->
      let dl, dr = if delay < 0. then 0., -.delay else delay, 0. in
      let x = delay_l ~delay:dl x in
      let y = delay_r ~delay:dr y in
      merge x y

  (** Schroeder reverberation. *)
  (* See https://ccrma.stanford.edu/~jos/pasp/Schroeder_Reverberators.html *)
  (* TODO: values are for 25 kHz sampling rate... *)
  let schroeder ~dt =
    let fbcf d g = comb ~dt d (-.g) in
    let ap = schroeder_allpass ~dt in
    (* Original values are given for a 25 kHz sampling rate .*)
    let ap1 = ap (347./.25000.) 0.7 in
    let ap2 = ap (113./.25000.) 0.7 in
    let ap3 = ap (37./.25000.) 0.7 in
    let fbcf1 = fbcf (1687./.25000.) 0.773 in
    let fbcf2 = fbcf (1601./.25000.) 0.802 in
    let fbcf3 = fbcf (2053./.25000.) 0.753 in
    let fbcf4 = fbcf (2251./.25000.) 0.733 in
    let mm x1 x2 x3 x4 = return (x1+.x3, x2+.x4) in
    fun x ->
      let i = ap1 x >>= ap2 >>= ap3 in
      let dupi,i = dup () i in
      dupi >> bind4 mm (i >>= fbcf1) (i >>= fbcf2) (i >>= fbcf3) (i >>= fbcf4)

  let schroeder_random ~dt =
    (* Feedback comb-filter. *)
    let fbcf d g = comb ~dt d (-.g) in
    let ap = schroeder_allpass ~dt in
    let ap1 = ap (Random.float 0.015 +. 0.001) 0.7 in
    let ap2 = ap (Random.float 0.015 +. 0.001) 0.7 in
    let ap3 = ap (Random.float 0.015 +. 0.001) 0.7 in
    let fbcf1 = fbcf (Random.float 0.05 +. 0.05) 0.773 in
    let fbcf2 = fbcf (Random.float 0.05 +. 0.05) 0.802 in
    let fbcf3 = fbcf (Random.float 0.05 +. 0.05) 0.753 in
    let fbcf4 = fbcf (Random.float 0.05 +. 0.05) 0.733 in
    let mm x1 x2 x3 x4 = return (x1+.x3, x2+.x4) in
    fun x ->
      let i = ap1 x >>= ap2 >>= ap3 in
      let dupi,i = dup () i in
      dupi >> bind4 mm (i >>= fbcf1) (i >>= fbcf2) (i >>= fbcf3) (i >>= fbcf4)

  let schroeder2 ~dt =
    let fbcf d g = comb ~dt d (-.g) in
    let ap = schroeder_allpass ~dt in
    let fbcf1 = fbcf (901./.25000.) 0.805 in
    let fbcf2 = fbcf (778./.25000.) 0.827 in
    let fbcf3 = fbcf (1011./.25000.) 0.783 in
    let fbcf4 = fbcf (1123./.25000.) 0.764 in
    let ap1 = ap (125./.25000.) 0.7 in
    let ap2 = ap (42./.25000.) 0.7 in
    let ap3 = ap (12./.25000.) 0.7 in
    let add4 x1 x2 x3 x4 = return (x1+.x2+.x3+.x4) in
    fun x ->
      bind4 add4 (fbcf1 x) (fbcf2 x) (fbcf3 x) (fbcf4 x) >>= ap1 >>= ap2 >>= ap3 >>= (fun x -> return (x, -.x))

  let agc ~dt () =
    let agcl = agc ~dt () in
    let agcr = agc ~dt () in
    map agcl agcr

  (** {2 Effects} *)

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
    (** Comb filter. *)
    let comb len =
      let buf = Array.make len 0. in
      let pos = ref 0 in
      let filterstore = ref 0. in
      fun input ->
        let output = buf.(!pos) in
        filterstore := output *. !damp2 +. !filterstore *. !damp1;
        buf.(!pos) <- input +. !filterstore *. !comb_feedback;
        incr pos;
        if !pos = len then pos := 0;
        return output
    in
    (** All-pass filter. *)
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
    fun  ?(roomsize=0.5) ?(damp=0.5) ?(width=1.) ?(wet=1./.3.) ?(dry=0.) (x,y) ->
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
      (* Apply filters. *)
      let i = (x +. y) *. gain in
      let combl = List.map (fun c -> c i) combl in
      let combr = List.map (fun c -> c i) combr in
      let outl = List.fold_left (funct2 (+.)) (return 0.) combl in
      let outr = List.fold_left (funct2 (+.)) (return 0.) combr in
      let outl = apl outl in
      let outr = apr outr in
      bind2 (fun outl outr ->
        let x = outl *. wet1 +. outr *. wet2 +. x *. dry in
        let y = outr *. wet1 +. outl *. wet2 +. y *. dry in
        return (x,y)) outl outr
end

let stereo = Stereo.of_mono
