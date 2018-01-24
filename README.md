gtp-measure
===
[![Build Status](https://travis-ci.org/bennn/gtp-measure.svg)](https://travis-ci.org/bennn/gtp-measure)
[![Scribble](https://img.shields.io/badge/Docs-Scribble-blue.svg)](http://docs.racket-lang.org/gtp-measure/index.html)

For benchmarking Racket programs.

The idea is to:

1. Have a plan of what to run
2. Recover when things don't go according to the plan

See the documentation for the details.


Usage
---

To start benchmarking `<TARGET>` with the default settings,
 or continue an interrupted run:

```
$ raco gtp-measure <TARGET>
```

To view all currently-running benchmarks:

```
$ raco gtp-measure --status
```

For more:

```
$ raco gtp-measure --help
```


History
---

<https://github.com/nuprl/gradual-typing-performance?path=tools/benchmark-run>
