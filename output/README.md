output
===

Output formats for `gtp-measure`.


### `#lang gtp-measure/output/file`

Output for a single file, contains a sequence of lines:
```
DATA
...
```

Where `DATA` is one of the following:
+ `(TIME-STR ...)`
  each `TIME-STR` reports CPU time, real time, and gc time
+ `timeout N`
  where `N` is a natural number; see `key:time-limit` in docs
+ `ERR-STR`
  a Racket runtime error message


### `#lang gtp-measure/output/typed-untyped`

Output for a mixed-typed program, contains a sequence of lines:

```
(CONFIG-STR FILE-DATA)
...
```

where `CONFIG-STR` is the name of a typed/untyped configuration
and `FILE-DATA` is a result for a single file (see above)

