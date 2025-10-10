(** OSC controllers. *)

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
  List.iter (fun (p,h) -> if p = path then List.iter h msg) !handlers

let server port =
  let server = Osc_unix.Udp.Server.create (Unix.ADDR_INET (Unix.inet_addr_any, port)) 1024 in
  ignore @@
  Thread.create
    (fun () ->
       while true do
         match Osc_unix.Udp.Server.recv server with
         | Ok (Message m, _) -> handler m.address m.arguments
         | Ok (Bundle _, _) -> ()
         | Error _ -> ()
       done
    ) ()

let register_float path f =
  let h = function
    | Osc.Types.Float32 x -> ts (fun () -> f x)
    | _ -> ()
  in
  add_handler path h

let register_bool path f =
  let h b = ts (fun () -> f b) in
  let h = function
    | Osc.Types.Float32 x -> h (x <> 0.)
    | Osc.Types.Int32 x -> h (x <> Int32.zero)
    | Osc.Types.Blob "false" -> h false
    | Osc.Types.Blob _ -> h true
    | _ -> ()
  in
  add_handler path h

open Stream

(* TODO: initialize sliders *)
let float ?mode ?min ?max path init =
  let x = ref init in
  let stretch = Math.stretch ?mode ?min ?max in
  let f x' = x := stretch x' in
  register_float path f;
  stream_ref x

let bool path init =
  let b = ref init in
  let f b' = b := b' in
  register_bool path f;
  stream_ref b
