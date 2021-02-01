#lang scribble/manual
@(require (for-label racket/base racket/require hash-view)
          scribble/example)

@title[#:version "1.0"]{hash-view: Struct-like Views of Hashes}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@(begin
   (define the-eval (make-base-eval))
   (the-eval '(require racket/match hash-view)))

@defmodule[hash-view]

@defform[(hash-view view-id (field-decl ...) maybe-mutability)
         #:grammar ([field-decl field-id
                                [field-id #:default default-expr]]
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

@examples[#:eval the-eval
(hash-view point (x y))
(point 1 2)
(point? (hasheq 'x 1))
(point? (point 1 2))
(point? (hash 'x 1 'y 2 'z 3))
(match (hasheq 'x 1 'y 2 'z 3)
  [(point x y) (+ x y)])
]

If a field has a default value, then it is an optional field: the predicate does
not check for its presence, and the accessor returns the given default value if
the field is absent. All fields with defaults must come after all required
fields. If a field is optional, then the corresponding constructor argument is
also optional.

@examples[#:eval the-eval
(hash-view location (host [port #:default 80]))
(location "racket-lang.org")
(location "docs.racket-lang.org" 443)
(location-port (location "racket-lang.org"))
(match (location "racket-lang.org")
  [(location host port) (format "~a:~a" host port)])
]

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
