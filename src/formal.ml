(** Formal streams. *)

(** Types. *)
module T = struct
  (** A type. *)
  type _ t =
    | Float : float t
    | Pair : 'a t * 'b t -> ('a * 'b) t
    | Unit : unit t

  (** Proofs of type equlaity. *)
  type (_, _) eq = Refl : ('a, 'a) eq

  (** Decide whether two types are equal. *)
  let rec decide : type a b . a t -> b t -> (a, b) eq option = fun x y ->
    match x, y with
    | Float, Float -> Some Refl
    | Float, _ | _, Float -> None
    | Unit, Unit -> Some Refl
    | Unit, _ | _, Unit -> None
    | Pair (a, b), Pair (a', b') ->
      (
        match decide a a', decide b b' with
        | Some Refl, Some Refl -> Some Refl
        | _ -> None
      )
end

(** Expressions. *)
module E = struct
  (** A formal expression. *)
  type _ t =
    | Var : (string * 'a T.t) -> 'a t
    | Fun : environment option * (string * 'a T.t) * 'b t -> ('a -> 'b) t
    | App : ('a -> 'b) t * 'a t -> 'b t
    | Float : float -> float t
    | Seq : unit t * 'a t -> 'a t
    | Add : float t * float t -> float t
    | Pair : 'a t * 'b t -> ('a * 'b) t
    | Unit : unit t

  (** A typed formal expression. *)
  (* aka "existential wrapper" *)
  and tt = TE : 'a T.t * 'a t -> tt

  (** A typing environment. *)
  and environment = (string * tt) list

  (** Evaluate an expression. *)
  let rec eval : type a . environment -> a t -> a t = fun env -> function
    | Var (x,a) as var ->
      (
        match List.assoc_opt x env with
        | Some (TE (a',t)) ->
          (
            match T.decide a a' with
            | Some Refl -> t
            | _ -> var
          )
        | _ -> var
      )
    | Fun (None, x, t) -> Fun (Some env, x, t)
    | Fun (Some env, x, t) -> Fun (Some env, x, t)
    | App (t, u) ->
      let t = eval env t in
      let u = eval env u in
      (
        match t with
        | Fun (Some env, (x, a), t) -> eval ((x,TE(a,u))::env) t
        | _ -> App (t, u)
      )
    | Float _ as t -> t
    | Seq (t, u) ->
      let t = eval env t in
      let u = eval env u in
      (
        match t with
        | Unit -> u
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

  (** Compute the value of an evaluated expression. *)
  let rec value : type a . a t -> a = function
    | Float x -> x
    | Unit -> ()
    | Var _ -> assert false
    | Fun _ -> assert false
    | App _ -> assert false
    | Seq _ -> assert false
    | Add _ -> assert false
    | Pair (t, u) -> (value t, value u)
end

module Stream = struct
  open E
  
  (** A stream basically is an expression encoding a function which takes dt and
      returns a float. *)
  type 'a t = (float -> 'a) E.t

  let dt = "dt", T.Float

  let value : 'a t -> 'a E.t = fun x -> App (x, Var dt)

  let return : 'a E.t -> 'a t = fun x -> Fun (None, dt, x)

  let float x : float t = return (Float x)

  let bind (f : 'a E.t -> 'b t) (x : 'a t) : 'b t = Fun (None, dt, App (f (value x), Var dt))

  let prod : 'a t -> 'b t -> ('a * 'b) t = fun x y -> return (E.Pair (value x, value y))

  module Operations = struct
    (** Return. *)
    let return = return

    (** Bind. *)
    let ( >>= ) x f = bind f x

    (** Bind with unit result. *)
    let ( >> ) x (f : unit t) = x >>= (fun _ -> f)

    (* (\** Functoriality. *\) *)
    (* let ( <$> ) = funct *)

    (* (\** Applicativity. *\) *)
    (* let ( <*> ) = apply *)

    (** Bind. *)
    let ( let* ) x f = bind f x

    (** Strength. *)
    let ( and* ) = prod
  end

  include Operations

  let dt : float t = Fun (None, dt, Var dt)
end
