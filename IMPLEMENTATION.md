# Events in the monad

One way to implement events is

```ocaml
type ('a, 'e) t = dt -> 'a * ('e -> unit)
```

so that for instance `integrate` has type

```
unit -> float -> (float , [< `Reset | `Set of float]) stream
```

The return is

```
let return : 'a -> ('a , 'e) t = fun x _ -> (x, default_handler)
```

(of course, we need a variant in order to additionally specify a handler) and
the bind is

```
let bind : ('a -> ('b , 'e) t) -> ('a, _) t -> ('b, 'e) t =
  fun f x dt -> f (fst (x dt)) dt
```

This looks nice, but we cannot access the effect handler, because it is under
the function taking `dt`: we need to create a new bind such as in

```
let** x, e = integrate s in
...
e `Reset
...
```

in order to get the effect handler and use it. Not very practical.

Another possible implementation is

```
type ('a, 'e) t = (dt -> 'a) * ('e -> unit)
```

where we have access to the emitter at toplevel. The return is now

```
let return : 'a -> ('a , 'e) t = fun x -> (fun _ -> x), default_handler
```

and the bind is

```
let bind : ('a -> ('b , 'e) t) -> ('a, _) t -> ('b, empty) t =
  fun f x -> (fun dt -> fst (f (fst x dt)) dt), default_handler
```

Note that we now loose access to the handler (we have `empty` as handler type)
when we have a source with parameters, this is not usable.

The first option means that handler can depend on all values, but can only be
used after a computation round. The second one is problematic because handlers
might a priori depend on parameters and we thus have to provide a value for
those.
