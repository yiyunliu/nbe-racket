#lang racket

(struct CLOS (env var body) #:transparent)

(define (extend rho x v)
  (cons (cons x v) rho))

;; type env = var -> value

;; (extend '() 'x 1)

;; env -> tm -> value
(define (val rho e)
  (match e
    [`(λ (,x) ,b)
     (CLOS rho x b)]
    [x #:when (symbol? x)
       (let ((xv (assv x rho)))
         (if xv
             (cdr xv)
             (error 'val "Unknown variable ~a" x)))]
    [`(,rator ,rand)
     (do-ap (val rho rator) (val rho rand))]))

;; env -> value -> value -> value
(define (do-ap clos arg)
  (match clos
    [(CLOS rho x b)
     (val (extend rho x arg) b)]))

;; Intuition: val and do-ap can be seen as an implementation of the ;;
;; big step semantics. Notably, the evaluator stops at a lambda term and
;; do not further evaluate under the binder

(define a-snd
  '(λ (x) (λ (y) y)))
(define a-fst
  '(λ (x) (λ (y) x)))
(define a-id
  '(λ (y) y))
(define a-pair
  '(λ (x) (λ (y) (λ (f) ((f x) y)))))

;; (val '() `(,a-fst ((,a-pair ,a-id) ,a-snd)))

(define (run-program rho exprs)
  (match exprs
    ['() (void)]
    [`((define ,x ,e) ,rest)
     (let ([v (val rho e)])
       (run-program (extend rho x v) rest))]
    [`(,e ,rest)
     (displayln (val rho e))
     (run-program rho rest)]))
