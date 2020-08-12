Monadic synthesizers
====================

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

## Our first sines

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

We use the function `sine` to create the oscillator, then use `>>=` to pipe it
to the `stereo` operator which converts a stream into a stereo one (a _stereo
stream_ is a stream of pairs of floats), and finally use `Output.play` to stream
the result. Another equivalent way to write this is

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

The last two syntaxes have the advantage of extending to functions with multiple
arguments.

One of the main advantage of using the monadic syntax is that all arguments can
vary over time. For instance, we can achieve a vibrato as follows:

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
  let left = sine () in
  let right = sine () in
  let s =
    let* x = left 440. in
    let* y = right 880. in
    pair x y
  in
  Output.play s
```

Here, we define the oscillators for the two channels (respectively called `left`
and `right`) and then define the stream `s` as the pair consisting of the
current value of the oscillators at respective frequencies 440 and 880 Hz. Note
that it is important that we create the oscillators beforehand: if we had
written

```ocaml
let () =
  let s =
    let* x = sine () 440. in
    let* y = sine () 880. in
    pair x y
  in
  Output.play s
```

## Using a stream multiple times

TODO: the problem, dup

## General principles behind the library

- Clean code is more important than efficient code (although we want to remain
  reasonably efficient).
- The infinitesimal variations are supposed to be varying slowly, i.e. be
  "locally constant". In particular, this means that small buffers can assume
  that the _dt_ is the same for the whole buffer.
