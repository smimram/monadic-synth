# Events in the monad

One way to implement events is

```ocaml
type ('a, 'e) t = dt -> 'a * ('e -> unit)
```

so that for instance `integrate` has type

```
unit -> float -> (float , [< `Reset | `Set of float]) stream
```

This looks nice, but we cannot access the effect handler, because it is under
the function taking `dt`: we need to create a new bind such as in

```
let** x, e = integrate s in
...
e `Reset
...
```

in order to get the effect handler and use it.
