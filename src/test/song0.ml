open Extlib
open Stream

let s =
  let note f ~event ~on_die () =
    let s = f () in
    let env = adsr ~event ~on_die () ~s:0.5 ~r:0.1 () in
    let denv, env = dup () env in
    fun freq vol ->
      let s = s freq in
      let s = mul env s in
      let s = denv >> bind2 (Filter.first_order () `Low_pass) (cmul 10000. env) s in
      let s = s >>= amp vol in
      s
  in
  let vm = 1. in
  let melody = [
    `Note_on (69,vm);
    `Note_off 69;
    `Note_on (72,vm);
    `Note_off 72;
    `Note_on (76,vm);
    `Note_off 76;
    `Note_on (72,vm);
    `Note_off 72;
  ]
  in
  let melody = List.mapi (fun n e -> 0.2 *. float n, e) melody in
  let melody = Instrument.play (note sine) melody in
  let melody = bind2 (Filter.first_order () `Low_pass) (add (cst 1000.) (cmul 300. (sine () 10.))) melody in
  let vb = 0.8 in
  let bass1 = [`Note_on (45,vb); `Note_off 45] in
  let bass2 = [`Note_on (41,vb); `Note_off 41] in
  let bass = (List.repeat 8 bass1)@(List.repeat 8 bass2)@[`Nop] in
  let bass = List.mapi (fun n e -> 0.2 *. float n, e) bass in
  let bass = Instrument.play (note saw) bass in
  let bass = bass >>= amp 0.5 in
  let bass = bass >>= Slicer.hachoir () 0.1 in
  let s = add melody bass in
  let s = s >>= Stereo.of_mono in
  let s = s >>= Stereo.delay () 0.3 ~feedback:0.4 ~ping_pong:0.2 in
  s >>= Stereo.amp 0.4

let () =
  Output.play s
