; Smallstep CEK machine, from section 2.1 / Figure 1 of
; "Abstracting Abstract Machines."
#lang racket

; Expressions

; Expressions are of the form:
; e ::= x
;    |  (e e)
;    |  (λ x e)
; 
; You can form them by using quotes in Racket:
; 'x
; 
; '(λ x e) <-- where e is some other expression
; for example '(λ x e)
; 
; '(e1 e2) <-- where e1 and e2 are also e's
; for example '((λ y y) (λ x x))
;
; Values are just:
; v ::= (λ x e)
; 
; I.e., they are the subset of expressions that are only *start* with
; lambdas. So '(λ x (λ y y)) is a value, but 'x is not.

; Contract to check expressions
(define (e? expr)
  (match expr
    [`,(? symbol? _) #t]
    [`(λ ,(? symbol? _) ,(? e? _)) #t]
    [`(,e1 ,e2) (and (e? e1) (e? e2))]
    [else #f]))

; Contract to check values
(define (v? v)
  (match v
    [`(λ ,(? symbol? _) ,(? e? _)) #t]))

; Environments

; Environments are implemented using hashes:
; https://docs.racket-lang.org/reference/hashtables.html

(define (closure? clo)
  (match clo
    [(cons v h) (and (v? v) (hash? h))]
    [else #f]))

(define empty-env (hash))

(define/contract (extend-env key clo env)
  (symbol? closure? hash? . -> . hash?)
  (hash-set env key clo))

(define (lookup key env)
  (hash? . -> . closure?)
  (hash-ref env key))

; (extend-env 'y (cons '(λ x x) empty-env) empty-env)

; Continuations

(struct mt ())
(struct ar (e rho kappa))
(struct fn (v rho kappa))

; Step function

(define (step sigma)
  (match sigma
    ; First rule in Fig 1
    [`(,(? symbol? x) ,rho ,kappa)
     (match-let
         ([(cons val rho2) `(,val ,rho2 ,kappa)])
       (lookup x rho))]))


       


