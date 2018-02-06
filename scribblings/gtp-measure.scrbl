#lang scribble/manual
@require[
  (for-label
    racket/base)]

@title[#:tag "top"]{GTP measure}
@author{Ben Greenman}

For benchmarking.


@; -----------------------------------------------------------------------------
@section{Command-line: @exec{raco gtp-measure}}

The @exec{gtp-measure} raco command is a tool for measuring the
 performance of a set of @tech{gtp-measure targets} according to a set
 of configuration options.
@margin-note{See also: @secref{gtp-measure-config}}
@; TODO add link for 'raco command'

A @deftech{gtp-measure target} is either:
@margin-note{See also: @secref{gtp-measure-targets}}
@itemlist[
@item{
  (@exec{-f}) a file containing a Racket module;
}
@item{
  (@exec{-t}) a directory containing typed/untyped code; or
}
@item{
  (@exec{-m}) a file containing a @racket[gtp-measure/manifest] module.
}
]

To see all accepted flags: @exec{raco gtp-measure --help}

To measure performance and print status messages:

@nested[#:style 'inset @exec{PLTSTDERR="error info@"@"gtp-measure" raco gtp-measure ....}]


@; -----------------------------------------------------------------------------
@section[#:tag "gtp-measure-targets"]{GTP targets}



@; -----------------------------------------------------------------------------
@section[#:tag "gtp-measure-config"]{GTP configuration}



@; -----------------------------------------------------------------------------
@;@section{Data Languages}
@;; TODO change name!!! global namespace!!!



@; -----------------------------------------------------------------------------
@;@section{Reference}
@; TODO change name!!!


