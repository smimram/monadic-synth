Monadic synthesizers in OCaml
=============================

This library called `msynth` is my own take at organizing the various classical
functions for performing audio synthesis. Our aim is to provide a clean
programming environment in which one can easily try new ideas for synthesizers
(performance is not a priority, although it is of course taken into account).

It is mainly based on the idea that audio streams can be represented as
functions `float -> float`: such a function takes as argument the time _dt_
elapsed since the last sample and returns the current value for the
sample. Typically, when the sampling rate is 44100, _dt_ will be 1/44100, but
fancy effects might vary its value. The type

```ocaml
type 'a stream = float -> 'a
```

is thus called a _stream_ of `'a`. It can be equipped with the structure of a
[monad](https://en.wikipedia.org/wiki/Monad_(functional_programming)) which can
be used to easily compose operations on streams, especially since the recent
introduction of the dedicated [syntax for
monads](https://caml.inria.fr/pub/docs/manual-ocaml/bindingops.html) in OCaml.

## Installing

The easiest way to install the library is to clone the [github
repository](https://github.com/smimram/monadic-synth) and type

```sh
opam pin add .
```

which will use opam to install the library and its dependencies.

## Documentation

The main documentation consists in

- this file,
- the [documentation generated from the source
  code comments](http://smimram.github.io/monadic-synth/ocamldoc/),
- the source code

# Tutorial

## Our first sines

### Playing a sound

When creating synthesizers, you typically want to open the `Stream` module:

```ocaml
open Stream
```

In subsequent code, we always suppose that this was done. Our first example
consists in playing a sine at 440Hz, which can be obtained with

```ocaml
let () =
  let s = sine () 440. >>= stereo in
  Output.play s
```

We use the function `sine` to create the oscillator (which oscillates 440 times
per second between -1 and 1), then use `>>=` to pipe it to the `stereo` operator
which converts a stream into a stereo one (a _stereo stream_ is a stream of
pairs of floats), and finally use `Output.play` to stream the result. Another
equivalent way to write this is

```ocaml
let () =
  let s =
    let* x = sine () 440. in
    stereo x
  in
  Output.play s
```

where the expression `let* x = s in e` can be interpreted as _given `x` the
current value of the stream `s` return `e`_. A last possible syntax (which is
less clear in our opinion) would be

```ocaml
let () =
  let s = bind stereo (sine () 440.) in
  Output.play s
```

Here, the `bind` operator has type

```ocaml
('a -> 'b t) -> ('a t -> 'b 't)
```

i.e. it transforms a function returning a stream so that it can take a stream as
argument.

### Modulating parameters

One of the main advantage of using the monadic syntax is that all arguments can
vary over time. For instance, we can achieve a vibrato as follows:

```ocaml
let () =
  let lfo = sine () in
  let vco = sine () in
  let s =
    let* f = lfo 5. in
    vco (440. +. 10. *. f)
  in
  Output.play (s >>= stereo)
```

Here, we begin by creating two oscillators respectively called `lfo` and `vco`
(the names come from the
[LFO](https://en.wikipedia.org/wiki/Low-frequency_oscillation) and
[VCO](https://en.wikipedia.org/wiki/Voltage-controlled_oscillator) electric
circuits) and state that the source `s` is the vco oscillator whose frequency is
around 440 Hz, varying by ±10 Hz at the rate of 5 Hz (the rate of the lfo). Note
that since the frequency is exponential with respect to notes, a vibrato of half
a semitone should rather be achieved by replacing the last line in the
definition of `s` by

```ocaml
    vco (440. *. 2. ** (0.5 *. f /. 12.))
```

but we leave this kind of details to you. Here, it is important that the
oscillators are created _beforehand_. If we try the code

```ocaml
let () =
  let s =
    let* f = sine () 5. in
    sine () (440. +. 10. *. f)
  in
  Output.play (s >>= stereo)
```

we do not hear any sound: this is because we create a new oscillator at each
sample, and thus always hear the first sample of the oscillator which is 0, and
this is not what we want. The general rule is: declare all operators before the
first `let*`.

Another way to write the same program as above, with the `>>=` operator, would
be

```ocaml
let () =
  let s = cadd 440. (cmul 10. (sine () 5.)) >>= sine () >>= stereo in
  Output.play s
```

_Exercise_: play a sine with tremolo, which can be achieved by periodically
varying its amplitude.

As another example, instead of generating a sine, we are going to generate a
square wave, using the `square` operator: by default, its value is 1 over half a
period and -1 over the other half. However, there is no particular reason to do
half and half, and we call the _width_ of a square wave, the portion of the
period its value is 1 (by default, the width is thus 0.5). We can achieve nice
sounds by periodically modulating this value, which is called [pulse width
modulation](https://en.wikipedia.org/wiki/Pulse-width_modulation). For instance:

```ocaml
let () =
  let lfo = sine () 2. in
  let osc = square () in
  let s =
    let* lfo = lfo in
    let width = 0.5 +. 0.3 *. lfo in
    osc ~width 440.
  in
  Output.play (s >>= stereo)
```

Here, we generate a square wave (`osc`) whose width is modulated between 0.2 and
0.8 by a sine oscillator (`lfo`) at the frequency of 2 Hz.

### Returning streams

We can create constant streams with the `return` function, which creates a
stream whose value is always the one given in the argument. For instance, we can
define the pairing function with

```ocaml
let pair x y = return (x, y)
```

Given two values `x` and `y`, it creates the stream whose value is always the
pair `(x, y)`. We can then use it to play a sine at different frequency over
each channel with

```ocaml
let () =
  let left  = sine () 440. in
  let right = sine () 880. in
  let s =
    let* x = left  in
    let* y = right in
    pair x y
  in
  Output.play s
```

Another possible way to write this is using the `bind2` operators whose type is

```ocaml
('a -> 'b -> 'c t) -> ('a t -> 'b t -> 'c t)
```

i.e. it transforms a function with two arguments which returns a stream, so that
it accepts streams as arguments:

```ocaml
let () =
  let left  = sine () 440. in
  let right = sine () 880. in
  let s = bind2 pair left right in
  Output.play s
```

### Parameters from OSC

One way to dynamically acquire parameters is to use the
[OSC](https://en.wikipedia.org/wiki/Open_Sound_Control) which is supported by
many software and hardware controllers (for instance, this [free app on
Android](https://play.google.com/store/apps/details?id=com.ffsmultimedia.osccontroller)). In
order to be able to use this, we should first call the function `OSC.server`
(which takes the port number on which it should listen as argument). The value
of a controller can then be acquired with the function `OSC.float`, which takes
as argument the path of the controller and its initial value and returns a
stream of its values.

For instance, in the following, we can play a saw oscillator chained with a
[low-pass filter](https://en.wikipedia.org/wiki/Low-pass_filter) where the
global volume, the [Q factor](https://en.wikipedia.org/wiki/Q_factor) and
[cutoff frequency](https://en.wikipedia.org/wiki/Cutoff_frequency) of the filter
can be configured though OSC controls as follows.

```ocaml
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
```

Here, we begin by creating the saw oscillator (`osc`) and the low-pass filter
(`lp`), as well as the streams corresponding to the controllers for
amplification, q and cutoff frequency. The line

```ocaml
let a = a >>= print "a" in
```

makes the value for amplification `a` being printed on standard output when it
changes, which is useful for debugging. Finally, the stream consists in the
oscillator which goes through the filter, is amplified, and finally converted to
stereo.

### Other examples

- [thx.ml](https://github.com/smimram/monadic-synth/blob/master/src/test/thx.ml):
  a quick recreation of the [THX deep
  note](https://www.youtube.com/watch?v=uYMpMcmpfkI) by adding many saw
  oscillators, whose frequency is initially between 200 and 400 Hz, and slowly
  evolve to the same note at various octaves.

## Instruments

....

TODO: ADSR on the envelope

### Arpeggiators

### Custom notes

TODO: ADSR on the filter

# Advanced topics

## Using a stream multiple times

Streams should not be used multiple times. For instance, if we want to play the
same sine on the left and the right channel, we might be tempted two write

```ocaml
let () =
  let pair x y = return (x, y) in
  let osc = sine () 440. in
  let s = bind2 pair osc osc in
  Output.play s
```

but the result is that we hear a sine at 880 Hz. The reason is that each time we
pull a sample for `s`, we actually pull two samples from `osc`: once for the
left channel and once for the right channel. One way to avoid this is to
explicitly extract the value of the stream, and there is no problem in
duplicated this value:

```ocaml
let () =
  let pair x y = return (x, y) in
  let osc = sine () 440. in
  let s =
    let* x = osc in
    pair x x
  in
  Output.play s
```

Another way consists in using the `dup` operator, which returns a pair: the
first one should be used first to evaluate the stream and the second one is a
stream which can be used as many times as we want. We can thus rewrite our
example as follows:

```ocaml
let () =
  let pair x y = return (x, y) in
  let osc = sine () 440. in
  let eval, osc = dup () osc in
  let s = eval >> bind2 pair osc osc in
  Output.play s
```

## General principles behind the library

- Clean code is more important than efficient code (although we want to remain
  reasonably efficient).
- The infinitesimal _dt_ is supposed to be very small. In particular, the stream
  at instant _t_ or _t+dt_ should be roughly the same.
- The infinitesimal variations are supposed to be varying slowly, i.e. be
  "locally constant". In particular, this means that small buffers can assume
  that the _dt_ is the same for the whole buffer.

## Ideas for the future

- Make a compiler (into, say, C) for our synthesizers, possibly by simply
  changing the monad.
