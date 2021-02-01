#lang racket/base
(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse/pre
                     racket/provide-transform
                     syntax/transformer)
         racket/match)
(provide hash-view
         hash-view-out)

(begin-for-syntax
  (struct hash-view-info (names-stx)
    ;; hvnames : Syntax[(name make make-mut name? (uc-name-f ...) (name-f ...))]
    #:property prop:procedure
    (lambda (self stx)
      (define/with-syntax (_ make _ ...) (hash-view-info-names-stx self))
      ((make-variable-like-transformer #'make) stx))
    #:property prop:match-expander
    (lambda (self stx)
      (define/with-syntax (name _ _ name? (uc-name-f ...) _)
        (hash-view-info-names-stx self))
      (syntax-parse stx
        [(_ fp ...)
         (unless (= (length (syntax->list #'(fp ...)))
                    (length (syntax->list #'(uc-name-f ...))))
           (wrong-syntax stx
                         "wrong number of fields for hash-view ~s\n  expected: ~e\n  got: ~e"
                         (syntax-e #'name)
                         (length (syntax->list #'(uc-name-f ...)))
                         (length (syntax->list #'(fp ...)))))
         #'(? name? (app uc-name-f fp) ...)])))
  (define (hash-view-info-all-names hvi)
    (with-syntax ([(name make make-mut name? (uc-get ...) (get ...))
                   (hash-view-info-names-stx hvi)])
      (filter identifier? (syntax->list #'(name make make-mut name? #| uc-get ... |# get ...))))))

(define-syntax (hash-view stx)
  (define-syntax-class fieldspec #:attributes (name mk-default ref-default [decl 1])
    (pattern name:id
             #:attr mk-default #f
             #:attr ref-default #f
             #:with (decl ...) null)
    (pattern [name:id #:default default0:expr]
             #:with (mk-default ref-default decl ...)
             (syntax-parse #'default0
               #:literals (quote)
               [(quote datum) #'(default0 default0)]
               [datum
                #:when (let ([d (syntax->datum #'datum)]) (or (boolean? d) (number? d)))
                #:when (free-identifier=? (datum->syntax #'datum '#%datum) #'#%datum)
                #'('default0 'default0)]
               [_ (with-syntax ([(tmp) (generate-temporaries #'(default))])
                    #'((tmp) tmp (define (tmp) default0)))])))
  (define-splicing-syntax-class mut-clause
    (pattern (~seq #:immutable) #:attr mode 'immutable)
    (pattern (~seq #:accept-mutable) #:attr mode 'accept-mutable)
    (pattern (~seq) #:attr mode 'accept-mutable))
  (syntax-parse stx
    [(_ name (f:fieldspec ...) mc:mut-clause)
     (for/fold ([seen-default? #f])
               ([f (in-list (syntax->list #'(f ...)))]
                [default (in-list (attribute f.ref-default))])
       (when (and seen-default? (not default))
         (raise-syntax-error #f "non-optional field following optional field" stx f))
       (or seen-default? (and default #t)))
     (with-syntax ([name? (format-id #'name "~a?" #'name)]
                   [make-name (format-id #'name "make-~a" #'name)]
                   [make-mut-name
                    (case (attribute mc.mode)
                      [(immutable) #f]
                      [(accept-mutable) (format-id #'name "make-mutable-~a" #'name)])]
                   [(uc-name-f ...)
                    (for/list ([fname (in-list (syntax->list #'(f.name ...)))])
                      (format-id #'HERE "unchecked-~a-~a" #'name fname))]
                   [(name-f ...)
                    (for/list ([fname (in-list (syntax->list #'(f.name ...)))])
                      (format-id #'name "~a-~a" #'name fname))]
                   [other-check
                    (case (attribute mc.mode)
                      [(immutable) #'(immutable? v)]
                      [else #'#t])])
       #'(begin
           f.decl ... ...
           (define (make-name (~? [f.name f.mk-default] f.name) ...)
             (hasheq (~@ 'f.name f.name) ...))
           (~? (define (make-mut-name (~? [f.name f.mk-default] f.name) ...)
                 (let ([h (make-hasheq)]) (hash-set! h 'f.name f.name) ... h)))
           (define-syntax name
             (hash-view-info
              (quote-syntax (name make-name (~? make-mut-name #f) name?
                                  (uc-name-f ...) (name-f ...)))))
           (define (name? v)
             (and (hash? v) other-check
                  (or (~? (begin (quote f.ref-default) #t)) (hash-has-key? v 'f.name)) ...
                  #t))
           (define (uc-name-f v) (hash-ref v 'f.name (~? f.ref-default))) ...
           (define (name-f v)
             (unless (name? v) (raise-argument-error 'name-f (symbol->string 'name?) v))
             (uc-name-f v))
           ...))]))

(define-syntax hash-view-out
  (make-provide-transformer
   (lambda (stx modes)
     (define (get-defined-ids)
       (hash-ref (syntax-local-module-defined-identifiers)
                 (syntax-local-phase-level)
                 null))
     (define (get-imported-ids)
       (define idss (or (syntax-local-module-required-identifiers #f #t) null))
       (cond [(assoc (syntax-local-phase-level) idss) => cdr] [else null]))
     (define (syntax-property-add id prop v)
       (cond [(syntax-property id prop) => (lambda (v0) (syntax-property id prop (cons v v0)))]
             [else (syntax-property id prop v)]))
     ;; ----
     (unless (or (null? modes) (equal? '(0) modes))
       (raise-syntax-error #f "allowed only for relative phase level 0" stx))
     (syntax-parse stx
       [(_ name)
        #:declare name (static hash-view-info? "hash view")
        (define avail-idss (list (get-defined-ids) (get-imported-ids)))
        (define hvids (hash-view-info-all-names (attribute name.value)))
        (define intersected-ids
          (for*/list ([avail-ids (in-list avail-idss)]
                      [avail-id (in-list avail-ids)]
                      #:when (for/or ([hvid (in-list hvids)])
                               (free-identifier=? hvid avail-id)))
            avail-id))
        (for/list ([hvid (in-list hvids)])
          (define avail (filter (lambda (id) (free-identifier=? id hvid)) intersected-ids))
          (define (none/multi which)
            (wrong-syntax this-syntax "~a for hash-view identifier: ~s" which hvid))
          (define id
            (cond [(null? avail) (none/multi "no binding")]
                  [(null? (cdr avail)) (car avail)]
                  [else (none/multi "multiple bindings")]))
          (make-export (syntax-property-add id 'disappeared-use (syntax-local-introduce #'name))
                       (syntax-e id)
                       0
                       #f
                       id))]))))
