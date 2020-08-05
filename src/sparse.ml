(** Sparse stream (= continuation) monad. *)

type 'a t = ('a -> unit) -> unit

let return : 'a -> 'a t =
  fun x k -> k x

let bind : ('a -> 'b t) -> 'a t -> 'b t =
  fun f x k -> x (fun x -> f x k)

let ( let* ) x f = bind f x

(*
let to_stream init (s : 'a t) =
  let x = ref init in
  let f x' = x := x' in
  s f;
  stream_ref x
*)
