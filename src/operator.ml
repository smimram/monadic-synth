(** Operators a stream generators. They consist of two combined monads: one for
    the state and one for streams (as in [Stream]). They should replace [Stream]
    if they prove practicable as one could forsee compiling those. *)

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

(** The outer monad. *)
type 'a t = state -> state * 'a value

let return x : 'a t =
  fun o -> o, x

let bind f x : ('a -> 'b t) -> 'a t -> 'b t =
  fun o -> f (x o) o

let ( let+ ) x f = bind f x

let alloc : 'a typ -> 'a value -> 'a reference t =
  fun t i o ->
  let cell = Array.append o.cell [|Type t|] in
  let n = Array.length cell - 1 in
  { o with cell }, Ref (t, n)

let alloc_float = alloc Float

let get : 'a reference -> 'a t =
  fun r -> return (Get r)

let set : 'a reference -> 'a value -> unit t =
  fun r x -> return (Set (r, x))

(** {2 The stream monad} *)

type dt = float

(** Pure streams. *)
type 'a stream = dt -> 'a
