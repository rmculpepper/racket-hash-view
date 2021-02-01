#lang info

;; pkg info

(define collection "hash-view")
(define deps '("base" "hash-view-lib"))
(define build-deps '("racket-doc" "scribble-lib"))
(define implies '("hash-view-lib"))
(define pkg-authors '(ryanc))

;; collection info

(define name "hash-view")
(define scribblings '(("hash-view.scrbl" ())))
