open Stream

let () =
  let s =
    let osc = saw () 440. in
    let lp  = Filter.biquad () `Low_pass in
    let a   = OSC.float "/oscControl/slider1" 0.5 in
    let lpq = OSC.float "/oscControl/slider2" ~min:0.1 ~max:5. 1. in
    let lpf = OSC.float ~mode:`Logarithmic "/oscControl/slider3" ~max:10000. 1500. in
    let a   = a   >>= print "a" in
    let lpq = lpq >>= print "q" in
    let lpf = lpf >>= print "f" in
    let* a  = a in
    let* f  = lpf in
    let* q  = lpq in
    osc
    >>= lp q f
    >>= amp a
    >>= stereo
  in
  OSC.server 10000;
  Output.play s

let () =
  let lfo = sine () 2. in
  let osc = square () in
  let s =
    let* lfo = lfo in
    let width = 0.5 +. 0.3 *. lfo in
    osc ~width 440.
  in
  Output.play (s >>= stereo)

let () =
  let pair x y = return (x, y) in
  let osc = sine () 440. in
  let eval, osc = dup () osc in
  let s = eval >> bind2 pair osc osc in
  Output.play s

let () =
  let pair x y = return (x, y) in
  let osc = sine () 440. in
  let s =
    let* x = osc in
    pair x x
  in
  Output.play s

let () =
  let pair x y = return (x, y) in
  let osc = sine () 440. in
  let s = bind2 pair osc osc in
  Output.play s

let pair x y = return (x, y)

let () =
  let left  = sine () 440. in
  let right = sine () 880. in
  let s = bind2 pair left right in
  Output.play s

let () =
  let left  = sine () 440. in
  let right = sine () 880. in
  let s =
    let* x = left  in
    let* y = right in
    pair x y
  in
  Output.play s

let () =
  let s = B.cadd 440. (B.cmul 10. (sine () 5.)) >>= sine () >>= stereo in
  Output.play s

let () =
  let s =
    let* f = sine () 5. in
    sine () (440. +. 10. *. f)
  in
  Output.play (s >>= stereo)

let () =
  let lfo = sine () in
  let vco = sine () in
  let s =
    let* f = lfo 5. in
    vco (440. +. 10. *. f)
    (* vco (440. *. 2. ** (0.5 *. f /. 12.)) *)
  in
  Output.play (s >>= stereo)

let () =
  let s = bind stereo (sine () 440.) in
  Output.play s

let () =
  let s =
    let* x = sine () 440. in
    stereo x
  in
  Output.play s

let () =
  let s = sine () 440. >>= stereo in
  Output.play s

