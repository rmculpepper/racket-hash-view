#lang racket/base
(require (for-syntax racket/base
                     racket/list
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
      (filter identifier? (syntax->list #'(name make make-mut name? #| uc-get ... |# get ...)))))

  (define (fix-formals formals)
    ;; Change every optional arg before the last required arg to be required.
    (define (required? formal) (identifier? formal))
    (define (optional? formal) (not (required? formal)))
    (define-values (prefix suffix) (splitf-at-right formals optional?))
    (append (map (lambda (formal)
                   (syntax-parse formal
                     [name:id #'name]
                     [[name:id _] #'name]))
                 prefix)
            suffix)))

(define-syntax (hash-view stx)
  (define-syntax-class field-declaration #:attributes (formal ex)
    (pattern name:id
             #:with formal #'name
             #:with ex #'(#:required name))
    (pattern [name:id #:default :defaultexpr]
             #:with formal #'[name mk]
             #:with ex #'(#:optional name #f mk ref decl ...))
    (pattern [name:id #:default/omit :defaultexpr]
             #:with formal #'[name mk]
             #:with ex #'(#:optional name #t mk ref decl ...)))
  (define-syntax-class defaultexpr
    #:attributes (mk ref [decl 1])
    (pattern e:expr
             #:with (tmp) (generate-temporaries '(tmp))
             #:with mk #'(tmp)
             #:with ref #'tmp
             #:with (decl ...) #'((define tmp (let ([tmp e]) (lambda () tmp))))))
  (define-syntax-class required-ex #:attributes (name)
    (pattern (#:required name)))
  (define-syntax-class optional-ex #:attributes (name omit? mk ref [decl 1])
    (pattern (#:optional name omit? mk ref decl ...)))
  (define-splicing-syntax-class mut-clause
    (pattern (~seq #:immutable) #:attr mode 'immutable)
    (pattern (~seq #:accept-mutable) #:attr mode 'accept-mutable)
    (pattern (~seq) #:attr mode 'accept-mutable))
  ;; ----
  (syntax-parse stx
    [(_ name (f:field-declaration ...) mc:mut-clause)
     #:with ((~alt rf:required-ex of:optional-ex) ...) #'(f.ex ...)
     #:attr make-mut-name (case (attribute mc.mode)
                            [(immutable) #f]
                            [(accept-mutable) (format-id #'name "make-mutable-~a" #'name)])
     (define fnamess (list (syntax->list #'(rf.name ...)) (syntax->list #'(of.name ...))))
     (with-syntax ([name? (format-id #'name "~a?" #'name)]
                   [make-name (format-id #'name "make-~a" #'name)]
                   [((uc-name-rf ...) (uc-name-of ...))
                    (for/list ([fnames (in-list fnamess)])
                      (for/list ([fname (in-list fnames)])
                        (format-id #'HERE "unchecked-~a-~a" #'name fname)))]
                   [((name-rf ...) (name-of ...))
                    (for/list ([fnames (in-list fnamess)])
                      (for/list ([fname (in-list fnames)])
                        (format-id #'name "~a-~a" #'name fname)))]
                   [other-check
                    (case (attribute mc.mode)
                      [(immutable) #'(immutable? v)]
                      [else #'#t])]
                   [(f-formal ...) (fix-formals (syntax->list #'(f.formal ...)))])
       #'(begin
           of.decl ... ...
           (define (make-name f-formal ...)
             (let* ([h (hasheq (~@ 'rf.name rf.name) ...)]
                    [h (cond [(and 'of.omit? (equal? of.name of.mk)) h]
                             [else (hash-set h 'of.name of.name)])]
                    ...)
               h))
           (~? (define (make-mut-name f-formal ...)
                 (let ([h (make-hasheq)])
                   (hash-set! h 'rf.name rf.name) ...
                   (unless (and 'of.omit? (equal? of.name of.mk))
                     (hash-set! h 'of.name of.name))
                   ...
                   h)))
           (define (name? v)
             (and (hash? v) other-check (hash-has-key? v 'rf.name) ... #t))
           (define (uc-name-rf v) (hash-ref v 'rf.name)) ...
           (define (uc-name-of v [default of.ref]) (hash-ref v 'of.name default)) ...
           (define (name-rf v)
             (unless (name? v) (raise-argument-error 'name-rf (symbol->string 'name?) v))
             (uc-name-rf v))
           ...
           (define (name-of v [default of.ref])
             (unless (name? v) (raise-argument-error 'name-of (symbol->string 'name?) v))
             (uc-name-of v default))
           ...
           (define-syntax name
             (hash-view-info
              (quote-syntax (name make-name (~? make-mut-name #f) name?
                                  (uc-name-rf ... uc-name-of ...)
                                  (name-rf ... name-of ...)))))))]))

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
