open Stream

class pulseaudio ?(channels=2) samplerate =
  let sample =
    { Pulseaudio.
      sample_format = Pulseaudio.Sample_format_float32le;
      sample_rate = samplerate;
      sample_chans = 2;
    }
  in
  let o = Pulseaudio.Simple.create ~client_name:"ocamlsynth" ~dir:Pulseaudio.Dir_playback ~stream_name:"sound" ~sample () in
object
  method write buf =
    Pulseaudio.Simple.write o buf 0 (Array.length buf.(0))

  method close = (* TODO *) ()
end

(*
class portaudio ?(channels=2) samplerate =
  let () = Portaudio.init () in
  let samplerate = float_of_int samplerate in
  let latency = 1. in
  let device = Portaudio.get_default_output_device () in
  let fmt = { Portaudio. channels; device; sample_format = Portaudio.format_float32; latency } in
  let stream = Portaudio.open_stream ~interleaved:false None (Some fmt) samplerate 0 [] in
object
  method write buf =
    Portaudio.write_stream stream buf 0 (Array.length buf.(0))

  method close = (* TODO *) ()
end
*)

class wav ?(channels=2) samplerate fname =
  let super = new Audio.IO.Writer.to_wav_file channels samplerate fname in
object
  method write buf =
    super#write buf 0 (Array.length buf.(0))

  method close = super#close
end

exception End_of_stream

let play ?(samplerate=44100) s =
  let dt = 1. /. float samplerate in
  Random.self_init ();
  let buflen = 1024 in
  let buf = Array.init 2 (fun _ -> Array.make buflen 0.) in
  let out = new pulseaudio samplerate in
  let wavout = new wav samplerate "output.wav" in
  let s = s ~dt in
  let dup_s, s = dup () s in
  let s = dup_s >> s >>= Stereo.to_mono >>= is_blank ~dt 2. 0.001 >>= activated () >>= on (fun () -> raise End_of_stream) >> s in
  try
    while true do
      for i = 0 to buflen - 1 do
        let (l,r) = s () in
        buf.(0).(i) <- l;
        buf.(1).(i) <- r
      done;
      out#write buf;
      wavout#write buf
    done;
  with
  | End_of_stream ->
    out#close;
    wavout#close
