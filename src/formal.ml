(** Formal streams. *)

(** Types. *)
module T = struct
  (** A type. *)
  type 'a t =
    | Float : float t
    | Pair : 'a t * 'b t -> ('a * 'b) t
    | Unit : unit t
end

(** Expressions. *)
module E = struct
  (** A formal expression. *)
  type t =
    | Var of string
    | Fun of environment option * string * t
    | App of t * t
    | Float of float
    | Seq of t * t
    | Add of t * t
    | Pair of t * t
    | Unit

  and environment = (string * t) list

  (** Evaluate an expression. *)
  let rec eval env = function
    | Var x as t -> (match List.assoc_opt x env with Some v -> v | None -> t)
    | Fun (None, x, t) -> Fun (Some env, x, t)
    | Fun (Some env, x, t) -> Fun (Some env, x, t)
    | App (t, u) ->
      let t = eval env t in
      let u = eval env u in
      (
        match t with
        | Fun (Some env, x, t) -> eval ((x,u)::env) t
        | _ -> App (t, u)
      )
    | Float _ as t -> t
    | Seq (t, u) ->
      let t = eval env t in
      let u = eval env u in
      (
        match t with
        | Unit -> u
        | Float _ -> assert false
        | _ -> Seq (t, u)
      )
    | Add (t, u) ->
      let t = eval env t in
      let u = eval env u in
      (
        match t, u with
        | Float x1, Float x2 -> Float (x1 +. x2)
        (* TODO: simplifications such as adding 0. *)
        | _ -> Add (t, u)
      )
    | Pair (t, u) -> Pair (eval env t, eval env u)
    | Unit -> Unit
end

module Stream = struct
  open E
  
  (** A stream is an expression encoding a function which takes dt and returns a
      float. *)
  type t = E.t

  let return x : t = Fun (None, "dt", x)

  let float x : t = return (Float x)

  let bind (f : t -> t) (x : t) : t =
    let dt = Var "dt" in
    let x = App (x, dt) in
    let y = f x in
    Fun (None, "dt", y)

  module Operations = struct
    (** Return. *)
    let return = return

    (** Bind. *)
    let ( >>= ) x f = bind f x

    (* (\** Bind with unit result. *\) *)
    (* let ( >> ) x f = x >>= (fun () -> f) *)

    (* (\** Functoriality. *\) *)
    (* let ( <$> ) = funct *)

    (* (\** Applicativity. *\) *)
    (* let ( <*> ) = apply *)

    (** Bind. *)
    let ( let* ) x f = bind f x

    (* (\** Strength. *\) *)
    (* let ( and* ) = prod *)
end

include Operations

  let dt : t =
    Fun (None, "dt", Var "dt")
end
