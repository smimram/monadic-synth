Monadic synthesizers
====================

This library called `msynth` is my own take at organizing the various classical
functions for performing audio synthesis.

It is mainly based on the idea that audio streams can be represented as
functions `float -> float`: such a function takes as argument the time _dt_
elapsed since the last sample and returns the current value for the
sample. Typically, when the sampling rate is 44100, _dt_ will be 1/44100 (but
fancy effects might vary its value). The operation which to the type `'a`
associates `float -> 'a` can be equipped with the structure of a
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

- this file
- the [documentation generated from the source
  code comments](http://smimram.github.io/monadic-synth/ocamldoc/)
- the source code

## Getting started

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
to the `stereo` operator which converts a stream into a stereo one, and finally
use `Output.play` to stream the result. Another equivalent way to write this
would be

```ocaml
let () =
  let s =
    let* x = sine () 440. in
    stereo x
  in
  Output.play s
```

## Using a stream multiple times

TODO: the problem, dup

## General principles behind the library

- The infinitesimal variations are supposed to be varying slowly, i.e. be
  "locally constant". In particular, this means that small buffers can assume
  that the _dt_ is the same for the whole buffer.
