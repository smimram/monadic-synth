(** Experimenting streams with state which we should both be able to execute and
    compile. Conclusion: this is very non-OCamlish since we have to replace every
    instruction by a "meta" (sequential composition, conditional branching,
    etc.). Implementing a proper dedicated compiler should be better. *)

open Extlib

module type Backend = sig
  type 'a t

  type 'a ref

  val float : float -> float t

  val seq : (unit -> unit t) -> 'a t -> 'a t

  val add : float t -> float t -> float t

  val mul : float t -> float t -> float t

  module Ref : sig
    val float : float t -> float ref t

    val get : 'a ref t -> 'a t

    val set : 'a ref t -> 'a t -> unit t
  end
end

module BackendOCaml : Backend = struct  
  type 'a t = 'a

  type nonrec 'a ref = 'a ref

  let float x = x

  let seq c x = c (); x

  let add x y = x +. y

  let mul x y = x *. y

  module Ref = struct
    let float (x : float t) : float ref = ref x

    let get x = !x

    let set x v = x := v
  end
end

module BackendFormal : Backend = struct  
  type 'a ref = int

  type 'a t =
    | Unit : unit t
    | Float : float -> float t
    | Seq : unit t * 'a t -> 'a t
    | Ref : 'a ref -> 'a ref t
    | Get : 'a ref t -> 'a t
    | Set : 'a ref t * 'a t -> unit t
    | Add : 'a t * 'a t -> 'a t
    | Mul : 'a t * 'a t -> 'a t

  let float x = Float x

  let seq c x = Seq (c (), x)

  let add x y = Add (x, y)

  let mul x y = Mul (x, y)

  module Ref = struct
    let floats = ref [||]

    let float =
      let n = Array.length !floats in
      fun x ->
        floats := Array.append !floats [|x|];
        Ref n

    let get x = Get x

    let set x v = Set (x, v)
  end
end

module Make(Backend : Backend) = struct
  open Backend

  let ( +. ) = add

  let ( *. ) = mul

  let ref = Ref.float

  let ( ! ) = Ref.get

  let ( := ) = Ref.set

  let ( >> ) = seq

  (** Type for time differences. *)
  type dt = float

  (** The stream monad. *)
  type 'a t = dt -> 'a

  (** Return operation of the stream monad. *)
  let return : 'a -> 'a t = fun x dt -> x

  (** Bind operation of the stream monad. *)
  let bind : ('a -> 'b t) -> 'a t -> 'b t =
    fun f x dt -> f (x dt) dt

  let ( !* ) = return

  let ( let* ) x f = bind f x

  (** Functoriality of the stream monad. *)
  let funct : ('a -> 'b) -> 'a t -> 'b t =
    fun f x ->
    let* x = x in
    !* (f x)

  (** Functoriality in two arguments of the stream monad. *)
  let funct2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t =
    fun f x y ->
    let* x = x in
    let* y = y in
    !* (f x y)

  (** Current infinitesimal variation of a stream. *)
  let dt : float t =
    fun dt -> dt

  (** Current value of a stream (this function might be removed in the future). *)
  let get : 'a t -> 'a =
    fun f -> f 0.

  (** {2 Pure operations} *)

  (** Forget the result of the stream (this is [ignore] for streams). *)
  let drop _ = !* ()

  (** Map a function on every sample of a stream. *)
  let map f x = !* (f x)

  (** Iterate a function on every sample of a stream. *)
  let iter f = map (fun x -> f x; x)

  (** Create a stream from a function indicating its value at each call. *)
  let seq f =
    let* _ = dt in
    !* (f ())

  (** Value of the stream at previous instant. *)
  let prev (x0:'a) =
    let prev = ref x0 in
    fun x ->
      let ans = !prev in
      (fun () -> prev := x) >>
      ans

  (** {2 Arithmetic} *)

  (** Create a constant stream. *)
  let cst x = return x

  (** The constantly zero stream. *)
  let blank = cst 0.

  let add = funct Float.add

  let sub = funct Float.sub

  let mul = funct Float.mul

  let div = funct Float.div

  (** Multiply a stream by a boolean (interpreted as 0 / 1 for false / true). *)
  let bmul b x =
    let* b = b in
    let* x = x in
    if b then return x else return 0.

  (** Switched multiplication by a constant: if the first is 0, the second
      stream is not evaluated. *)
  let smul x s =
    let* x = x in
    (* TODO: this should actually be a "meta" if *)
    if x = float 0. then return (float 0.)
    else
      let* y = s in
      return (x *. y)

  (** Number of samples in a given amount of time. *)
  let samples t =
    let* dt = dt in
    !* (round (t /. dt))

  (** {2 Time} *)

  (** Integrate a stream. *)
  let integrate ?(kind=`Euler) ?(init=float 0.) ?(periodic=false) () =
    let y = ref init in
    let return ans =
      (* if periodic && !y >= 1. then (y := !y -. 1.; on_reset ()); *)
      return ans
    in
    match kind with
    | `Euler ->
      fun x ->
        let* dt = dt in
        let ans = !y in
        (* y := !y +. x *. dt; *)
        (* return ans *)
        failwith "TODO.........."
    (* | `Trapezoidal -> *)
      (* let u = ref 0. in *)
      (* fun x -> *)
        (* let* dt = dt in *)
        (* let ans = !y in *)
        (* y := !u +. x /. 2.; *)
        (* u := !u +. x; *)
        (* return ans *)

end

(*
type 'a typ =
  | Float : float typ
  (* | Bool : bool kind *)
  | Pair : ('a typ * 'b typ) -> ('a * 'b) typ

(* existential type *)
type types =
  | Type : 'a typ -> types

type 'a reference = 'a typ * int

type 'a value =
  | Unit : unit value
  | Float : float -> float value
  | Ref : 'a reference -> 'a reference value
  | Get : 'a reference -> 'a value
  | Set : 'a reference * 'a value -> unit value

(** {2 The outer monad (which is roughly the indexed state monad) } *)

(** State of an operator. *)
type state =
  {
    cell : types array; (** all allocated cells *)
  }

type dt = float

(** The monad. *)
type 'a t = state -> state * (dt -> 'a)

let return : 'a -> 'a t = fun x st -> st, fun dt -> dt

let bind : ('a -> 'b t) -> 'a t -> 'b t = fun f x ->
  fun st ->
    let st, x = x st in
    let st, _ = f (x 0.) st in
    st,
    fun dt ->
      let _, y = f (x dt) st in
      y dt

let ( let* ) x f = bind f x

(* let alloc : 'a typ -> 'a value -> 'a reference t = *)
  (* fun t i o -> *)
  (* let cell = Array.append o.cell [|Type t|] in *)
  (* let n = Array.length cell - 1 in *)
  (* { o with cell }, Ref (t, n) *)

(* let alloc_float = alloc Float *)

(* let get : 'a reference -> 'a t = *)
  (* fun r -> return (Get r) *)

(* let set : 'a reference -> 'a value -> unit t = *)
  (* fun r x -> return (Set (r, x)) *)

(* (\** {2 The stream monad} *\) *)


(* (\** Pure streams. *\) *)
(* type 'a stream = dt -> 'a *)
*)
