(** Formal streams. *)

(** Types. *)
module T = struct
  (** A type. *)
  type _ t =
    | Float : float t
    | Pair : 'a t * 'b t -> ('a * 'b) t
    | Unit : unit t

  type ex_t = Ex : 'a t -> ex_t

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

  (** Whether two types are equal. *)
  let eq : type a b . a t -> b t -> bool = fun x y -> decide x y <> None
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
    | Ref : string * 'a T.t -> 'a ref t
    | Get : 'a ref t -> 'a t
    | Set : 'a ref t * 'a t -> unit t

  (** A typed formal expression. *)
  (* aka "existential wrapper" *)
  and tt = TE : 'a T.t * 'a t -> tt

  (** A typing environment. *)
  and environment = (string * tt) list

  (** A state. *)
  type state = (string * tt ref) list

  (** Compute the value of an evaluated expression. *)
  let rec value : type a . a t -> a = function
    | Float x -> x
    | Unit -> ()
    | Var _ -> assert false
    | Fun _ -> assert false
    | App _ -> assert false
    | Seq _ -> assert false
    | Add _ -> assert false
    | Ref _ -> assert false
    | Get _ -> assert false
    | Set _ -> assert false
    | Pair (t, u) -> (value t, value u)

  let is_value : type a . a t -> bool = fun x ->
    try ignore (value x); true
    with _ -> false

  (** Evaluate an expression. *)
  let rec eval : type a . state -> environment -> a t -> a t = fun state env -> function
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
      let t = eval state env t in
      let u = eval state env u in
      (
        match t with
        | Fun (Some env, (x, a), t) -> eval state ((x,TE(a,u))::env) t
        | _ -> App (t, u)
      )
    | Float _ as t -> t
    | Seq (t, u) ->
      let t = eval state env t in
      let u = eval state env u in
      (
        match t with
        | Unit -> u
        | _ -> Seq (t, u)
      )
    | Add (t, u) ->
      let t = eval state env t in
      let u = eval state env u in
      (
        match t, u with
        | Float x1, Float x2 -> Float (x1 +. x2)
        (* TODO: simplifications such as adding 0. *)
        | _ -> Add (t, u)
      )
    | Pair (t, u) -> Pair (eval state env t, eval state env u)
    | Unit -> Unit
    | Ref _ as t -> t
    | Get r ->
      let r = eval state env r in
      (
        match r with
        | Ref (r',a) ->
          (
            match List.assoc_opt r' state with
            | Some { contents = TE (a', t) } ->
              (
                match T.decide a a' with
                | Some Refl -> t
                | None -> assert false
              )
            | None -> Get r
          )
        | _ -> Get r
      )
    | Set (t, u) ->
      let t = eval state env t in
      let u = eval state env u in
      (
        match t with
        | Ref (r',a) ->
          (
            match List.assoc_opt r' state with
            | Some ({ contents = TE (a', _) } as r) ->
              (
                match T.decide a a' with
                | Some Refl -> r := TE (a, u); Unit
                | None -> assert false
              )
            | None -> Set (t, u)
          )
        | _ -> Set (t, u)
      )
end

module Stream = struct
  open E
  
  (** A stream basically is an expression encoding a function which takes dt and
      returns a float. We also provide the list of defined references along with
      their type. *)
  type 'a t = (float -> 'a) E.t * (string * T.ex_t) list

  let expr : 'a t -> (float -> 'a) E.t = fst

  let refs : 'a t -> (string * T.ex_t) list = snd

  let dt = "dt", T.Float

  (** Current value of a stream. *)
  let value : 'a t -> 'a E.t = fun x -> App (expr x, Var dt)

  (** Return an expression as a constant stream. *)
  let return ?(refs=[]) (x : 'a E.t) : 'a t = Fun (None, dt, x), refs

  (** Constant float stream. *)
  let float x : float t = return (Float x)

  let bind (f : 'a E.t -> 'b t) (x : 'a t) : 'b t =
    let y = f (value x) in
    Fun (None, dt, value y), (refs x)@(refs y)

  let prod (x : 'a t) (y : 'b t) : ('a * 'b) t =
    return ~refs:((refs x)@(refs y)) (E.Pair (value x, value y))

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

  let dt : float t = Fun (None, dt, Var dt), []
end
