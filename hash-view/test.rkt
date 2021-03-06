#lang racket/base
(require rackunit
         racket/match
         hash-view)

;; ----------------------------------------

(module point racket/base
  (require hash-view)
  (provide (hash-view-out point))
  (hash-view point (x y [z #:default/omit 0])))
(require 'point)

(define p3 (point 1 2 3))
(check-pred point? p3)
(check-pred point? (point 1 2))
(check-pred point? (hash 'x 1 'y 2))
(check-pred point? (hash 'x 1 'y 2 'color "red"))
(check-equal? (point? (hasheq 'x 1)) #f)
(check-equal? (match p3 [(point x y z) (+ x y z)]) 6)
(check-equal? (match (point 1 2) [(point x y z) (+ x y z)]) 3)

;; Check the representation of #:default/omit field:
(check-equal? (point 1 2) (hasheq 'x 1 'y 2))
(check-equal? (point 1 2 0) (hasheq 'x 1 'y 2))
(check-equal? (point 1 2 3) (hasheq 'x 1 'y 2 'z 3))

;; ----------------------------------------

(hash-view loc (host [port #:default 80]))

(check-pred loc? (loc "racket-lang.org"))
(check-pred loc? (loc "racket-lang.org" 443))
(check-pred loc? (hasheq 'host "racket-lang.org" 'scheme "ftp"))

(check-equal? (loc-host (loc "racket-lang.org")) "racket-lang.org")
(check-equal? (loc-host (loc "racket-lang.org" 443)) "racket-lang.org")
(check-equal? (loc-port (loc "racket-lang.org")) 80)
(check-equal? (loc-port (loc "racket-lang.org" 443)) 443)

;; Check the representation of #:default field:
(check-equal? (loc "racket-lang.org") (hasheq 'host "racket-lang.org" 'port 80))
(check-equal? (loc "racket-lang.org" 80) (hasheq 'host "racket-lang.org" 'port 80))
(check-equal? (loc "racket-lang.org" 443) (hasheq 'host "racket-lang.org" 'port 443))

;; ----------------------------------------

;; Check opt field before req is required arg in constructor:
(hash-view optreq ([opt #:default/omit #f] req))
(check-equal? (procedure-arity optreq) 2)
(check-equal? (optreq 1 2) (hasheq 'opt 1 'req 2))
