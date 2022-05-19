(** Extensions to standard library. *)

(** Don't do anything. *)
let nop () = ()

(** Round to the nearest integer. *)
let round x = int_of_float (x +. 0.5)

module List = struct
  include List

  let make n x = init n (fun _ -> x)

  let repeat n l =
    let rec aux k =
      if k = 0 then [] else l@(aux (k-1))
    in
    aux n

  let last l =
    let rec aux = function
      | [x] -> x
      | _::t -> aux t
      | [] -> raise Not_found
    in
    aux l

  let rev_iter f l =
    let rec aux = function
      | x::t -> aux t; f x
      | [] -> ()
    in
    aux l

  let compose l x =
    let x = ref x in
    List.iter (fun f -> x := f !x) l;
    !x
end

module File = struct
  let to_string fname =
    let ic = open_in fname in
    let ans = ref "" in
    let buflen = 1024 in
    let buf = Bytes.create buflen in
    let n = ref 1 in
    while !n <> 0 do
      n := input ic buf 0 buflen;
      ans := !ans ^ String.sub (Bytes.unsafe_to_string buf) 0 !n
    done;
    !ans
end

module Complex = struct
  include Complex

  let make re im = { re; im }

  let real x = make x 0.

  let imaginary x = make 0. x

  let re c = c.re

  let im c = c.im

  let cmul a c = { re = a *. c.re; im = a *. c.im }
end

module Float = struct
  include Float

  let of_bool = function
    | false -> 0.
    | true -> 1.
end

module Random = struct
  include Random

  let float ?(min=0.) max = Random.float (max -. min) +. min
end
