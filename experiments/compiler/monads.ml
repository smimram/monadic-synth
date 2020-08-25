(** Testing simple usual monads. *)

(** {2 The state monad} *)

type mem

type nonrec 'a state = mem -> mem * 'a

let return : 'a -> 'a state = fun x st -> st, x

let bind : ('a -> 'b state) -> 'a state -> 'b state = fun f x st ->
  let st, x = x st in
  f x st

(** {2 The stream monad} *)

type dt = float

type 'a stream = dt -> 'a

let return : 'a -> 'a stream = fun x dt -> x

let bind : ('a -> 'b stream) -> 'a stream -> 'b stream = fun f x dt ->
  f (x dt) dt

type 'a t = mem -> mem * (dt -> 'a)

let return : 'a -> 'a t = fun x st -> st, fun dt -> dt

let bind : ('a -> 'b t) -> 'a t -> 'b t = fun f x ->
  fun st ->
    let st, x = x st in
    let st, _ = f (x 0.) st in
    st,
    fun dt ->
      let _, y = f (x dt) st in
      y dt
