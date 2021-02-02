#lang scribble/manual
@(require (for-label racket/base racket/match hash-view)
          scribble/example)

@title[#:version "1.0"]{hash-view: Struct-like Views of Hashes}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@(begin
   (define the-eval (make-base-eval))
   (the-eval '(require racket/match hash-view)))

@defmodule[hash-view]

This library provides a form for declaring a struct-like interface to hashes
with symbol keys.

@; ------------------------------------------------------------
@section[#:tag "intro"]{Introduction to Hash Views}

A hash view is declared like this:
@examples[#:eval the-eval #:label #f
(hash-view point (x y))
]

This declares a constructor and match pattern @racket[point], a predicate
@racket[point?], and accessors @racket[point-x] and @racket[point-y].

Instances of points are represented by hashes with symbol keys:
@examples[#:eval the-eval #:label #f
(point 1 2)
(point? (point 1 2))
(point? 12)
]
All of the (required) fields must be present to satisfy the
predicate. Additional keys are allowed and ignored.
@examples[#:eval the-eval #:label #f
(point? (hasheq 'x 1))
(point? (hash 'x 1 'y 2 'z 3))
]

The hash-view name also acts as a @racket[match] pattern:
@examples[#:eval the-eval #:label #f
(match (hasheq 'x 1 'y 2 'z 3)
  [(point x y) (+ x y)])
]

A hash-view declaration can include optional fields with default values. Fields
declared with @racket[#:default] are included by the constructor, but fields
declared with @racket[#:default/omit] are omitted when they have their default
values.
@examples[#:eval the-eval #:label #f
(hash-view location (host [port #:default 80] [proto #:default/omit 'tcp]))
(location "racket-lang.org")
(location "racket-lang.org" 80 'tcp)
(location "docs.racket-lang.org" 443)
(location "localhost" 20 'udp)
]

An optional field's accessor normally returns the declared default if the field
is missing:
@examples[#:eval the-eval #:label #f
(location-port (hasheq 'host "racket-lang.org"))
(match (hasheq 'host "racket-lang.org")
  [(location host port _) (format "~a:~a" host port)])
]
But if the accessor is given a second argument, that argument replaces the
default failure argument to @racket[hash-ref]:
@examples[#:eval the-eval #:label #f
(location-port (hasheq 'host "racket-lang.org")
               443)
(location-port (hasheq 'host "racket-lang.org")
               (lambda () (printf "port missing!\n")))
]


@; ------------------------------------------------------------
@section[#:tag "api"]{Hash Views}

@defform[(hash-view view-id (field-decl ...) maybe-mutability)
         #:grammar ([field-decl field-id
                                [field-id #:default default-expr]
                                [field-id #:default/omit default-expr]]
                    [maybe-mutability (code:line)
                                      #:immutable
                                      #:accept-mutable])]{

Similar to @racket[(struct view-id (field-id ...))], but represents
@racket[view-id] instances as hashes with symbol keys (@racket['field-id]).

Defines @racketvarfont{view-id} as a constructor and match pattern,
@racketvarfont{view-id}@litchar{?} as a predicate, and the
@racketvarfont{view-id}@litchar{-}@racketvarfont{field-id} identifiers as
accessors.

The @racketvarfont{view-id} constructor always creates immutable
@racket[eq?]-based hashes (@racket[hasheq]), but the predicate and accessors
also accept @racket[eqv?]-based and @racket[equal?]-based hashes
(@racket[hasheqv] and @racket[hash]). The predicate and accessors accept hashes
that have additional keys.

If a field has a default value, then it is an optional field: the predicate does
not check for its presence. An accessor for an optional field takes an optional
second argument that is used as the failure argument for the @racket[hash-ref]
call; if the second argument is omitted, the accessor returns the declared
default value if the field is absent. All fields with defaults must come after
all required fields. If a field is optional, then the corresponding constructor
argument is also optional. If the field is declared with @racket[#:default], the
constructor always includes the field in the hash; if the default is declared
with @racket[#:default/omit], the constructor omits the field when the
corresponding argument is the same (@racket[equal?]) to the default value.

If the @racket[#:immutable] option is given, then the predicate and accessors
accept only immutable hashes as instances of @racket[view-id]. If the
@racket[#:accept-mutable] option is given, then the predicate and accessors
accept both mutable and immutable hashes, and a
@litchar{make-mutable-}@racketvarfont{view-id} function is defined that creates
mutable instances (using @racket[make-hasheq]). If neither option is given, the
behavior defaults to the @racket[#:accept-mutable] behavior.
}

@defform[(hash-view-out hash-view-id)]{

Like @racket[struct-out] but provides the identifiers associated with the given
hash-view.
}

@(close-eval the-eval)
