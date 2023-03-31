#lang racket

(struct CLOS (env var body) #:transparent)

(define (extend rho x v)
  (cons (cons x v) rho))

;; type env = var -> value

;; (extend '() 'x 1)


;; Variable not bound by substitution
(struct N-var (name))

;; An application where the function is neutral
(struct N-ap (rator rand))

;; env -> tm -> value
(define (val rho e)
  (match e
    [`(λ (,x) ,b)
     (CLOS rho x b)]
    ;; What if there is a bound x that's never applied?
    ;; That situation will never happen because we never
    ;; evaluate under a lambda unless there is an applicant
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
     (val (extend rho x arg) b)]
    [neutral-fun
     (N-ap neutral-fun arg)]))

(define (add-* x)
  (string->symbol (string-append (symbol->string x) "*")))

(define (freshen used x)
  (if (memv x used)
      (freshen used (add-* x))
      x))

;; value -> tm
(define (read-back used v)
  (match v
    [(CLOS rho x b)
     (let* ([y (freshen used x)]
            [neutral-y (N-var y)])
       `(λ (,y)
          ;; Note that we need another read-back here to typecheck
          ,(read-back (cons y used) (val (extend rho x neutral-y) b))))]
    [(N-var x) x]
    [(N-ap f a) `(,(read-back used f) ,(read-back used a))]))


;; Intuition: val and do-ap can be seen as an implementation of the ;;
;; big step semantics. Notably, the evaluator stops at a lambda term and
;; do not further evaluate under the binder

(define a-snd
  '(λ (p) (p (λ (x) (λ (y) y)))))
(define a-fst
  '(λ (p) (p (λ (x) (λ (y) x)))))
(define a-id
  '(λ (y) y))
(define a-pair
  '(λ (x) (λ (y) (λ (f) ((f x) y)))))

;; (read-back '() (val '() `(,a-fst ((,a-pair ,a-snd) ,a-id))))

(define (run-program rho exprs)
  (match exprs
    ['() (void)]
    [`((define ,x ,e) ,rest)
     (let ([v (val rho e)])
       (run-program (extend rho x v) rest))]
    [`(,e ,rest)
     (displayln (val rho e))
     (run-program rho rest)]))

(define (norm rho e)
  (read-back '() (val rho e)))
