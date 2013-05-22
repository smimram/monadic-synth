open Stream

let m = Mutex.create ()

let handlers = ref []

let add_handler path h =
  handlers := (path,h) :: !handlers

let ts f =
  try
    Mutex.lock m;
    f ();
    Mutex.unlock m
  with
  | e ->
    Mutex.unlock m;
    raise e

let handler path msg =
  let msg = Array.to_list msg in
  List.iter (fun (p,h) -> if p = path then List.iter h msg) !handlers

let server port =
  let server = LO.Server.create port handler in
  ignore (Thread.create (fun () -> while true do LO.Server.recv server done) ())

let register_float path f =
  let h = function
    | `Float x | `Double x -> ts (fun () -> f x)
    | _ -> ()
  in
  add_handler path h

let register_bool path f =
  let h b = ts (fun () -> f b) in
  let h = function
    | `Float x | `Double x -> h (x <> 0.)
    | `False -> h false
    | `True -> h true
    | _ -> ()
  in
  add_handler path h

let stretch ~mode ~min ~max =
  let d = max -. min in
  match mode with
  | `Linear ->
    (fun x -> x *. d +. min),
    (fun x -> (x -. min) /. d)
  | `Logarithmic ->
    (fun x ->
      let x = (10. ** x -. 1.) /. 9. in
      x *. d +. min
    ),
    (fun x ->
      let x = (x -. min) /. d in
      log10 (x *. 9. +. 1.))

(* TODO: initialize sliders *)
let float ?(mode=`Linear) ?(min=0.) ?(max=1.) path init =
  let x = ref init in
  let stretch, stretch_inv = stretch ~mode ~min ~max in
  let f x' = x := stretch x' in
  register_float path f;
  stream_ref x

let bool path init =
  let b = ref init in
  let f b' = b := b' in
  register_bool path f;
  stream_ref b
