#lang scribble/manual

@; TODO
@; - document the output format

@require[
  (submod gtp-measure/private/configure for-doc)
  gtp-measure/configure
  (only-in gtp-measure/private/configure config-ref)
  (only-in racket/format ~a ~s)
  scriblib/autobib
  scribble/example
  (for-label
    basedir
    gtp-util
    racket/base
    gtp-measure/configure
    racket/contract
    (only-in typed/racket require/typed)
    require-typed-check)]

@(define gtp-measure-exec @exec{raco gtp-measure --help})

@title[#:tag "top"]{GTP measure}
@author{Ben Greenman}

@defmodule[gtp-measure]{
  For benchmarking.
}


@; -----------------------------------------------------------------------------
@section{Command-line: @exec{raco gtp-measure}}

The @exec{gtp-measure} raco command is a tool for measuring the
 performance of a set of @tech{gtp-measure targets} according to a set
 of configuration options.
@margin-note{See also: @secref{gtp-measure-targets}}
@margin-note{See also: @secref{gtp-measure-config}}
@; TODO add link for 'raco command'

To see all accepted flags: @|gtp-measure-exec|

To measure performance and print status messages:

@nested[#:style 'inset @exec{PLTSTDERR="error info@"@"gtp-measure" raco gtp-measure ....}]


@subsection{Stages of measurement}

After @exec{gtp-measure} is invoked on the command line, it operates in five stages:

@itemlist[
@item{
  resolve the command-line @seclink["gtp-measure-targets"]{targets} to actual files / directories;
}
@item{
  resolve the command-line configuration options to a @seclink["gtp-measure-config"]{configuration};
}
@item{
  @seclink["gtp-measure-setup"]{setup} a @seclink["gtp-measure-task"]{measuring task}, based on the targets;
}
@item{
  divide the measuring task into @seclink["gtp-measure-subtask"]{sub-tasks};
}
@item{
  collect data, write to the task's @seclink["gtp-measure-data"]{data directory}.
}
]


@subsection[#:tag "gtp-measure-data"]{Configuration and Data Files}

The @racketmodname[gtp-measure] library uses the @racketmodname[basedir] library
 to obtain configuration and data files.

User-level configuration settings are stored in the file:

@racketblock[
  (writable-config-file "config.rktd" #:program "gtp-measure")
]

Each @seclink["gtp-measure-task"]{task} gets a data directory, stored under:

@racketblock[
  (writable-data-dir #:program "gtp-measure")
]

Together, the data files and command-line arguments build a
@tech{gtp-measure configuration} value.
See @secref["gtp:config-fallback"] for details on how these data sources
 work together.


@; -----------------------------------------------------------------------------
@section[#:tag "gtp-measure-targets"]{GTP targets}

A @deftech{gtp-measure target} is either:
@itemlist[
@item{
  @deftech{gtp file target} :
  a file containing a Racket
   module and exactly one call to @racket[time-apply]
   (possibly via @racket[time]);
}
@item{
  @deftech{gtp typed/untyped target} :
  a directory
   containing: (1) a @filepath{typed} directory, (2) an @filepath{untyped} directory,
   (3) optionally a @filepath{base} directory, and (4) optionally a @filepath{both} directory.
  @itemlist[
  @item{
    The @filepath{typed} directory must contain a few @racketmodname[typed/racket] modules.
  }
  @item{
    The @filepath{untyped} directory must contain matching Racket modules.
    These modules must have the same name as the modules in the @filepath{typed} directory,
     and should have the same code as the typed modules --- just missing type annotations and type casts.
  }
  @item{
    The optional @filepath{base} directory may contain data files that the
     @filepath{typed} and @filepath{untyped} modules may reference via a
     relative path (e.g. @filepath{../base/file.rkt})
  }
  @item{
    The optional @filepath{both} directory may contain modules that the
     @filepath{typed} and @filepath{untyped} modules may reference as if they
     were in the same directory (e.g. @filepath{file.rkt}).
    If so, the @filepath{typed} and @filepath{untyped} modules will not compile
     unless the @filepath{both} modules are copied into their directory.
    This is @seclink["gtp-typed/untyped-design"]{by design}.
  }
  ]
}
@item{
  @deftech{gtp manifest target} :
  a file containing a @racketmodname[gtp-measure/manifest] module.
}
@item{
  @deftech{gtp deep/shallow/untyped target} :
  a directory
   containing: (1) a @filepath{typed} directory, (2) an @filepath{untyped} directory,
   (3) a @filepath{shallow} directory,
   (4) optionally a @filepath{base} directory, and (5) optionally a @filepath{both} directory.
  @itemlist[
  @item{
    The @filepath{typed} and @filepath{untyped} directories must follow the same
    guidelines as for a @tech[#:key "gtp-typed/untyped-target"]{typed/untyped target}.
  }
  @item{
    The @filepath{shallow} directory must contain matching typed modules
    in Transient mode (@tt{#lang typed/racket #:transient}).
  }
  @item{
    The optional @filepath{base} directory may contain data files that the
     @filepath{typed} and @filepath{untyped} modules may reference via a
     relative path (e.g. @filepath{../base/file.rkt})
  }
  @item{
    The optional @filepath{both} directory may contain modules that the
     @filepath{typed} and @filepath{untyped} modules may reference as if they
     were in the same directory (e.g. @filepath{file.rkt}).
    If so, the @filepath{typed} and @filepath{untyped} modules will not compile
     unless the @filepath{both} modules are copied into their directory.
    This is @seclink["gtp-typed/untyped-design"]{by design}.
  }
  ]
}
]

To measure a @tech[#:key "gtp-file-target"]{file target},
 @racketmodname[gtp-measure] compiles the file once and repeatedly: runs the
 file and parses the output of @racket[time-apply].
See @secref{gtp-measure-config} for details on how @racketmodname[gtp-measure]
 compiles and runs Racket modules.

To measure a @tech[#:key "gtp-typed/untyped-target"]{typed/untyped target},
 @racketmodname[gtp-measure] chooses a sequence of
 @tech{typed/untyped configurations} and, for each: copies the configuration to
 a directory, and runs this program's
 @tech[#:key "gtp-entry-module"]{entry module} as a file target.
The sequence of configurations is either
 @tech[#:key "gtp-measure-exhaustive"]{exhaustive}
 or @tech[#:key "gtp-measure-approximate"]{approximate}.

To measure a manifest target, @racketmodname[gtp-measure] runs the targets
 listed in the manifest.

To measure a @tech[#:key "gtp-deep/shallow/untyped-target"]{deep/shallow/untyped target},
 the protocol is similar to @tech[#:key "gtp-typed/untyped-target"]{typed/untyped targets}.


@subsection{Typed/Untyped Configuration}

A @deftech{typed/untyped configuration} for a
 @tech[#:key "gtp-typed/untyped-target"]{typed/untyped target} with @math{M}
 modules is a working program with @math{M} modules --- some typed (maybe none),
 some untyped.

The @racketmodname[gtp-measure] library encodes such a configuration with
 a string of length @math{M} where each character is either
 @racket[#\0] or @racket[#\1].
If the character at position @math{i} is @racket[#\0], the configuration uses
 the @math{i}-th module in the @filepath{untyped} directory and ignores the
 @math{i}-th module in the @filepath{typed} directory.
If the character at position @math{i} is @racket[#\1], the configuration uses
 the @math{i}-th @filepath{typed} module and ignores the @filepath{untyped}
 module.
Modules are ordered by @racket[filename-sort].


@subsection{Exhaustive vs. Approximate evaluation}
@(define-cite ~ea-cite ea-citet ea-genbib)
@(define gtnffvf-jfp-2017
   (make-bib
    #:author (authors "Ben Greenman"
                      "Asumu Takikawa"
                      "Max S. New"
                      "Daniel Feltey"
                      "Robert Bruce Findler"
                      "Jan Vitek"
                      "Matthias Felleisen")
    #:title "How to Evaluate the Performance of Gradual Type Systems"
    #:location "Submitted for publication"
    #:date 2017))
@(define gm-pepm-2018
   (make-bib
     #:title "On the Cost of Type-Tag Soundness"
     #:author (authors "Ben Greenman" "Zeina Migeed")
     #:location (proceedings-location "PEPM" #:pages '(30 39))
     #:date 2018))

An @deftech[#:key "gtp-measure-exhaustive"]{exhaustive evaluation} of a
 @tech[#:key "gtp-typed/untyped-target"]{typed/untyped target} with @math{M}
 modules measures the performance of all @math{2^M} configurations.
This is a lot of measuring, and will probably take a very long time if @math{M}
 is @math{15} or more.

An @math{R-S}-@deftech[#:key "gtp-measure-approximate"]{approximate evaluation}
 measures @math{R * S * M} randomly-selected configurations; more precisely,
 @math{R} sequences containing @math{S*M} configuration in each sequence.
@; Hmm... sequence could be "ordered set" if sampling without replacement,
@; ... and in principle order shouldn't matter but it MIGHT because of hardware?
This number, @math{RSM}, is probably less than @math{2^M}.
(If it's not, just do an exhaustive evaluation.)
See @secref{gtp-measure-config} for how to set @math{R} and @math{S},
 and how to switch from an exhaustive evaluation to an approximate one.

The idea of an approximate evaluation comes from @hyperlink["http://www.ccs.neu.edu/home/types/publications/pe4gt/gtnffvf-jfp-2016.pdf"]{our work on Typed Racket}.
@hyperlink["http://www.ccs.neu.edu/home/types/publications/publications.html#gm-pepm-2018"]{Greenman and Migeed (PEPM 2018)}
 give a more precise definition, and apply the idea to Reticulated Python.
Note that @racketmodname[gtp-measure] uses a different definition of @math{S}
 than the PEPM paper.


@subsection[#:tag "gtp-typed/untyped-design"]{Design: typed/untyped directory}

The point of a @tech[#:key "gtp-typed/untyped-target"]{typed/untyped directory}
 is to describe an exponentially-large set of programs in ``less than exponential'' space.
The set is all ways of taking a Typed Racket program and removing some of its
 types --- specifically, removing types from some of the modules in the program.
So given a typed/untyped directory, @racketmodname[gtp-measure] needs to be able
 to generate and run each program.

The @filepath{typed} and @filepath{untyped} directories are a first step to
 reduce space.
Instead of storing all @math{2^M} programs for a program with @math{M}
 modules, we store @math{2M} modules.
The reason we store @math{2M} instead of just @math{M} typed modules is that
 we do not have a way to automatically remove types from a Typed Racket program
 (to remove types, we sometimes want to translate type casts to Racket).

The @filepath{base} directory is a second way to save space.
If a program depends on data or libraries, they belong in the @filepath{base}
 directory so that all configurations can reference one copy.

The @filepath{both} directory helps us automatically generate configurations by solving a technical problem.
The problem is that if an untyped module defines a
 struct and two typed modules import it, both typed modules need to reference
 a canonical @racket[require/typed] for the struct's type definitions.
We solve this by putting an @deftech{type adaptor module} with the @racket[require/typed]
 in the @filepath{both} directory.
An adaptor can require @filepath{typed} or @filepath{untyped} modules, and
 typed modules can require the adaptor.


@; -----------------------------------------------------------------------------
@section[#:tag "gtp-measure-config"]{GTP configuration}

@defmodule[gtp-measure/configure]

The @racketmodname[gtp-measure] library is parameterized by a set of key/value
 pairs.
This section documents the available keys and the type of values each key expects.

@defthing[gtp-measure-config/c flat-contract?]{
  Contract for a @deftech{gtp-measure configuration}; that is,
   an immutable hash whose keys are a subset of those documented
   below and whose values match the descriptions below.
}

@defidform[#:kind "symbol" key:bin]{
  Value must be a string that represets a path to a directory.
  The directory must contain executables named @exec{raco} and @exec{racket}.

  Used to compile and run Racket programs.

  In particular, if @litchar{<BIN>} is the value of @racket[key:bin] then the
   command to compile the target @litchar{<FIILE>} is:

  @nested[#:style 'inset @litchar{<BIN>/raco make -v <FILE>}]

  and the command to run @litchar{<FILE>} is:

  @nested[#:style 'inset @litchar{<BIN>/racket <FILE>}]

  Since this package was originally created to measure the
   @hyperlink["https://docs.racket-lang.org/gtp-benchmarks/index.html"]{GTP benchmarks},
   which depend on the @racketmodname[require-typed-check] package,
   invoking @exec{raco gtp-measure} ensures that the package is installed for
   the current value of @racket[key:bin].
  If the package is missing, @litchar{<BIN>/raco pkg} installs it.

  @history[#:changed "0.3" @elem{Automatically install @racketmodname[require-typed-check] if missing.}]
}

@defidform[#:kind "symbol" key:iterations]{
  Value must be an @racket[exact-positive-integer?].

  Determines the number of times to run a @tech[#:key "gtp-file-target"]{file target}
   and collect data.
}

@defidform[#:kind "symbol" key:jit-warmup]{
  Value must be an @racket[exact-nonnegative-integer?].

  Determines the number of times (if any) to run a @tech[#:key "gtp-file-target"]{file target}
   and ignore the output BEFORE collecting data.
}

@defidform[#:kind "symbol" key:num-samples]{
  Value must be an @racket[exact-positive-integer?]

  Determines @math{R}, the number of samples for any
   @tech[#:key "gtp-measure-approximate"]{approximate} evaluations.
}

@defidform[#:kind "symbol" key:sample-factor]{
  Value must be an @racket[exact-positive-integer?]

  Determines the size of each sample in any
   @tech[#:key "gtp-measure-approximate"]{approximate} evaluations.
  The size is @math{S*M}, where @math{S} is the value associated with this key
   and @math{M} is the number of modules in the @tech[#:key "gtp-typed/untyped-target"]{typed/untyped target}.
}

@defidform[#:kind "symbol" key:cutoff]{
  Value must be an @racket[exact-nonnegative-integer?].

  Determines whether to run an exhaustive or approximate evaluation for a
   @tech[#:key "gtp-typed/untyped-target"]{typed/untyped target}.
  Let @math{M} be the number of modules in the target and let @math{C} be the
   value associated with this key.
  If @racket[(<= M C)], then @racket[gtp-measure] runs an exhaustive evaluation;
   otherwise, it runs an approximate evaluation.
}

@defidform[#:kind "symbol" key:entry-point]{
  Value must be a string that represents a filename.

  Determines the @deftech[#:key "gtp entry module"]{entry module} of all
   @tech[#:key "gtp-typed/untyped-target"]{typed/untyped targets}.
  This module is treated as a @tech[#:key "gtp-file-target"]{file target} for
   each configuration in the typed/untyped evaluation.
}

@defidform[#:kind "symbol" key:start-time]{
  Value must be a real number.

  By default, this is the value of @racket[current-inexact-milliseconds] when
   @racketmodname[gtp-measure] was invoked.
  You should probably not override this default.
}

@defidform[#:kind "symbol" key:time-limit]{
  Value must be an @racket[(or/c #false exact-nonnegative-integer?)].

  Sets a time limit for the total time to run a configuration.
  If the value is @racket[#false] then there is no time limit.
  Otherwise, the value is the time limit in @bold{seconds}.

  The total time includes all the warmup iterations and all the collecting iterations.

  See also @secref{sec:parse-time-limit}.

  @history[#:added "0.3"]
}

@defidform[#:kind "symbol" key:argv]{
  Value must be a list of string.

  By default, this is the value of @racket[(vector->list (current-command-line-arguments))]
   when @racketmodname[gtp-measure] was invoked.
  You should probably not override this default.
}

@defidform[#:kind "symbol" key:working-directory]{
  Value must be a string absolute path.

  All intermediate files and all results are saved in the given directory.
}


@subsection[#:tag "gtp:config-fallback"]{Configuration Fallback}

The @racketmodname[gtp-measure] library defines a default value for each
 configuration key.
Users can override this default by writing a hashtable with relevant keys
 (a subset of the keys listed above) to their
 @seclink["gtp-measure-data"]{configuration file}.
Users can override both the defaults and their global configuration by
 supplying a command-line flag.
Run @|gtp-measure-exec| to see available flags.

The defaults for the machine that rendered this document are the following:

@(apply itemlist
  (for/list ([k-stx (in-list (list @racket[key:bin] @racket[key:iterations] @racket[key:jit-warmup] @racket[key:num-samples] @racket[key:sample-factor] @racket[key:cutoff] @racket[key:entry-point] @racket[key:start-time] @racket[key:time-limit] @racket[key:argv]))]
             [k-val (in-list (list key:bin key:iterations key:jit-warmup key:num-samples key:sample-factor key:cutoff key:entry-point key:start-time key:time-limit key:argv))])
    @item{@|k-stx| = @racket[(unsyntax @config-ref[DEFAULT-CONFIG k-val])]}))


@; -----------------------------------------------------------------------------
@section[#:tag "gtp-measure-task"]{GTP measuring task}

A task describes a sequence of targets to measure.


@subsection[#:tag "gtp-measure-setup"]{GTP task setup}

Before measuring the targets in a task, the @racketmodname[gtp-measure] library
 allocates a directory for the task and writes files that describe what is to
 be run.
If the task is interrupted, @racketmodname[gtp-measure] may be able to resume
 the task; run @|gtp-measure-exec| for instructions.


@subsection[#:tag "gtp-measure-subtask"]{GTP sub-task}

A sub-task is one unit of a task.
This concept is not well-defined.
The idea is to divide measuring tasks into small pieces so there is little to
 recompute if a task is interrupted.

More later.


@; -----------------------------------------------------------------------------
@section{Data Description Languages}

The @racketmodname[gtp-measure] library includes a few small languages to
 describe data formats.

@subsection{Manifest = Benchmark Instructions}

@defmodulelang[gtp-measure/manifest]{
  A @tech[#:key "gtp-manifest-target"]{manifest} contains an optional hash with configuration options
   and a sequence of target descriptors.

  The configuration options must be prefixed by the keyword @racket[#:config]
   and must be a hash literal that matches the @racket[gtp-measure-config/c]
   contract.
  If present, the options specified in the hash override any defaults.

  A target descriptor is either a string representing a file or directory,
   or a pair of such a string and a target kind.
  In the first case, the target kind is inferred at runtime.
  In the second case, the target kind is checked at runtime.
}

@codeblock[#:keep-lang-line? #true]{
  #lang gtp-measure/manifest

  #:config #hash((iterations . 10))

  file-0.rkt
  typed-untyped-dir-0
  "file-1.rkt"
  ("file-2.rkt" . file)
  (typed-untyped-dir-1 . typed-untyped)
}


There is an internal syntax class for these ``target descriptors'' that should
 be made public.


@subsection{Output Data: File Target}

@defmodulelang[gtp-measure/output/file]{
  Output data for one @tech{gtp file target}.

  Each line contains a result for one iteration of the file.
  The result may be:
  @itemize[
  @item{
    successful @racket[time] output, containing the CPU time, real time, and GC time;
  }
  @item{
    a Racket runtime error message;
  }
  @item{
    or a timeout notice (@racket{timeout N}).
  }]

}

@subsection{Output Data: Typed/Untyped Target}

@defmodulelang[gtp-measure/output/typed-untyped]{
  Output data for a @tech{gtp typed/untyped target}.

  Each line is the result for one configuration.
  The first element is the name of the configuration;
   the second is a sequence of file results.

  Provides an identifier @racketid[gtp-data] that is
   bound to the full dataset.

  Example data from a benchmark that ran with no timeouts or errors:

@codeblock[#:keep-lang-line? #true]{
#lang gtp-measure/output/typed-untyped
("00000" ("cpu time: 566 real time: 567 gc time: 62" "cpu time: 577 real time: 578 gc time: 62"))
("00001" ("cpu time: 820 real time: 822 gc time: 46" "cpu time: 793 real time: 795 gc time: 44"))
("00010" ("cpu time: 561 real time: 562 gc time: 46" "cpu time: 565 real time: 566 gc time: 44"))
("00011" ("cpu time: 805 real time: 807 gc time: 47" "cpu time: 813 real time: 815 gc time: 45"))
....
}
}

@subsection{Output Data: Deep/Shallow/Untyped Target}

@defmodulelang[gtp-measure/output/deep-shallow-untyped]{
  Output data for a @tech{gtp deep/shallow/untyped target}.

  Each line is the result for one configuration.
  The first element is the name of the configuration;
   the second is a sequence of file results.

  Provides an identifier @racketid[gtp-data] that is
   bound to the full dataset.

  Example data from a benchmark that ran with no timeouts or errors:

@codeblock[#:keep-lang-line? #true]{
#lang gtp-measure/output/deep-shallow-untyped
("00000" ("cpu time: 325 real time: 325 gc time: 60"))
("00001" ("cpu time: 336 real time: 336 gc time: 64"))
("00002" ("cpu time: 332 real time: 332 gc time: 64"))
("00010" ("cpu time: 7059 real time: 7061 gc time: 70"))
("00020" ("cpu time: 410 real time: 410 gc time: 64"))
("00011" ("cpu time: 7119 real time: 7121 gc time: 76"))
("00012" ("cpu time: 7035 real time: 7037 gc time: 76"))
("00021" ("cpu time: 426 real time: 426 gc time: 63"))
("00022" ("cpu time: 433 real time: 433 gc time: 77"))
("00100" ("cpu time: 7154 real time: 7158 gc time: 80"))
....
}
}


@; -----------------------------------------------------------------------------
@section{gtp-measure Utilities}

@subsection[#:tag "sec:parse-time-limit"]{Time Limit Parsing}

@defproc[(string->time-limit [str string?]) exact-nonnegative-integer?]{
  Convert a string to a natural number that represents a time limit in seconds.
  The string must begin with a sequence of digits (@litchar{[0-9]*})
   and may optionally end with one of the following unit suffixes:
   @racket{h} (hours),
   @racket{m} (minues),
   @racket{s} (seconds, the default).

  @examples[#:eval (make-base-eval '(require gtp-measure/parse))
    (string->time-limit "1")
    (string->time-limit "1s")
    (string->time-limit "1m")
    (string->time-limit "1h")
  ]
}

@deftogether[(
  @defproc[(hours->seconds [h exact-nonnegative-integer?]) exact-nonnegative-integer?]{}
  @defproc[(minutes->seconds [m exact-nonnegative-integer?]) exact-nonnegative-integer?]{}
)]{
  Convert a positive amount of time (in some kind of units) to an equal amount of seconds.

  @examples[#:eval (make-base-eval '(require gtp-measure/parse))
    (hours->seconds 1)
    (minutes->seconds 1)
  ]
}




