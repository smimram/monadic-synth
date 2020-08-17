open Stream

(*
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
    method buflen = 1024

    method write buf =
      Pulseaudio.Simple.write o buf 0 (Array.length buf.(0))

    method close = (* TODO *) ()
  end
*)

class alsa ?(channels=2) samplerate =
  let open Alsa in
  let dev, period_size =
    let buflen = 1024 in
    let periods = 4 in
    let dev = Pcm.open_pcm "default" [Pcm.Playback] [] in
    let params = Pcm.get_params dev in
    Pcm.set_access dev params Pcm.Access_rw_interleaved;
    Pcm.set_format dev params Pcm.Format_float;
    let _ = Pcm.set_rate_near dev params samplerate Dir_eq in
    Pcm.set_channels dev params channels;
    Pcm.set_buffer_size dev params buflen;
    Pcm.set_periods dev params periods Dir_eq;
    Pcm.set_params dev params;
    let period_size = Pcm.get_period_size params in
    Pcm.prepare dev;
    dev, period_size
  in
  let ba = Bigarray.Array1.create Bigarray.Float32 Bigarray.C_layout (channels * period_size) in
  object (self)
    method buflen = period_size

    method write buf =
      let buflen = self#buflen in
      for i = 0 to buflen - 1 do
        for c = 0 to channels - 1 do
          ba.{channels*i+c} <- buf.(c).(i)
        done
      done;
      try ignore (Pcm.writei_float_ba dev channels ba)
      with
      | Alsa.Buffer_xrun ->
        Printf.eprintf "ALSA: buffer xrun\n%!";
        Pcm.prepare dev

    method close = ()
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
  let oc = open_out fname in
  object (self)
    initializer
      let bits_per_sample = 16 in
      self#output "RIFF";
      self#output_int 0;
      self#output "WAVE";
      (* Format *)
      self#output "fmt ";
      self#output_int 16;
      self#output_short 1;
      self#output_short channels;
      self#output_int samplerate;
      self#output_int (samplerate * channels * bits_per_sample / 8);
      self#output_short (channels * bits_per_sample / 8);
      self#output_short bits_per_sample;
      (* Data *)
      self#output "data";
      (* size of the data, to be updated afterwards *)
      self#output_short 0xffff;
      self#output_short 0xffff

    method private output s = output_string oc s

    method private output_num b n =
      let s = Bytes.create b in
      for i = 0 to b - 1 do
        Bytes.set s i (char_of_int ((n lsr (8 * i)) land 0xff))
      done;
      self#output (Bytes.to_string s)

    method private output_byte n = self#output_num 1 n

    method private output_short n = self#output_num 2 n

    method private output_int n = self#output_num 4 n

    method private output_short_float x =
      let x = min 32767 (max (-32767) (int_of_float (x *. 32767.))) in
      self#output_short x

    method write buf =
      assert (Array.length buf = channels);
      for i = 0 to Array.length buf.(0) - 1 do
        for c = 0 to channels - 1 do
          self#output_short_float buf.(c).(i)
        done
      done

    method close : unit =
      close_out oc
  end

exception End_of_stream

let play ?(samplerate=44100) ?duration s =
  let s =
    match duration with
    | None -> s
    | Some t -> at () t >>= on (fun () -> raise End_of_stream) >> s
  in
  let dt = 1. /. float samplerate in
  Random.self_init ();
  let out = new alsa samplerate in
  let buflen = out#buflen in
  let buf = Array.init 2 (fun _ -> Array.make buflen 0.) in
  let wavout = new wav samplerate "output.wav" in
  try
    while true do
      for i = 0 to buflen - 1 do
        let l, r = s dt in
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
