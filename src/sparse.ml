(** Sparse streams. *)

(** A sparse stream, which runs a continuation whenever there is a stream to
    play. *)
type 'a t = ('a -> unit) -> unit

(** A constant stream. *)
let return : 'a -> 'a t =
  fun x k -> k x

let bind : ('a -> 'b t) -> 'a t -> 'b t =
  fun f x k -> x (fun x -> f x k)

(** Build a stream from a sparse stream. *)
let to_stream init (s : 'a t) =
  let x = ref init in
  let f x' = x := x' in
  s f;
  Stream.stream_ref x
