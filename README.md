Monadic synthesizers in OCaml
=============================

This library called `msynth` is my own take at organizing the various classical
functions for performing audio synthesis.

It is mainly based on the idea that audio streams can be represented as
functions `float -> float`: such a function takes as argument the time _dt_
elapsed since the last sample and returns the current value for the
sample. Typically, when the sampling rate is 44100, _dt_ will be 1/44100 (but
fancy effects might vary its value). The type

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

Our first example consists in playing a sine at 440Hz, which can be obtained
with

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
sample, and thus always hear the first sample of the oscillator which is 0,
and this is not what we want.

Another way to write the same program as above, with the `>>=` operator, would
be

```ocaml
let () =
  let s = cadd 440. (cmul 10. (sine () 5.)) >>= sine () >>= stereo in
  Output.play s
```

_Exercise_: play a sine with tremolo, which can be achieved by periodically
varying its amplitude.

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

One way to dynamically acquire parameters

[OSC](https://en.wikipedia.org/wiki/Open_Sound_Control)

## Instruments

....

# Advanced topics

## Using a stream multiple times

Streams should not be used multiple times. For instance, if we want to play the
same sine on the left and the right channel, we might be tempted two write

```ocaml
let () =
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
