#lang plait

(module+ test
  (print-only-errors #t))

;; abstract syntax -------------------------------

(define-type Op-unary
  (neg)
  (unm))

(define-type Op-binary
  (add)
  (sub)
  (mul)
  (div)
  (eql)
  (leq)
  (cnj)
  (dsj))

(define-type Exp
  (numE [n : Number])
  (opE-unary  [op : Op-unary]
              [arg : Exp])
  (opE-binary [op : Op-binary]
              [l : Exp]
              [r : Exp])
  (ifE [b : Exp]
       [l : Exp]
       [r : Exp])
  (condE [cs : (Listof (Exp * Exp))]))

;; parse ----------------------------------------

(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s)
     (numE (s-exp->number s))]
    [(s-exp-match? `{if ANY ANY ANY} s)
     (ifE (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s)))
          (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{cond ANY ...} s)
     (condE (parse-cond (rest (s-exp->list s))))]
    [(s-exp-match? `{SYMBOL ANY} s)
     (opE-unary  (parse-op-unary (s-exp->symbol (first (s-exp->list s))))
          (parse (second (s-exp->list s))))]
    [(s-exp-match? `{SYMBOL ANY ANY} s)
     (opE-binary (parse-op-binary (s-exp->symbol (first (s-exp->list s))))
          (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(define (parse-cond [ss : (Listof S-Exp)]) : (Listof (Exp * Exp))
  (type-case (Listof S-Exp) ss
    [empty
     empty]
    [(cons s ss)
     (if (s-exp-match? `{ANY ANY} s)
         (cons (pair (parse (first (s-exp->list s)))
                     (parse (second (s-exp->list s))))
               (parse-cond ss))
         (error 'parse "invalid input: cond"))]))

(define (parse-op-unary  [op : Symbol]) : Op-unary
  (cond
    [(eq? op '~) (neg)]
    [(eq? op '-) (unm)]
    [else (error 'parse "unknown operator")]))

(define (parse-op-binary [op : Symbol]) : Op-binary
  (cond
    [(eq? op '+) (add)]
    [(eq? op '-) (sub)]
    [(eq? op '*) (mul)]
    [(eq? op '/) (div)]
    [(eq? op '=) (eql)]
    [(eq? op '<=) (leq)]
    [(eq? op '∧) (cnj)]
    [(eq? op '∨) (dsj)]
    [else (error 'parse "unknown operator")]))
                
(module+ test
  (test (parse `2)
        (numE 2))
  (test (parse `{+ 2 1})
        (opE-binary (add) (numE 2) (numE 1)))
  (test (parse `{* 3 4})
        (opE-binary (mul) (numE 3) (numE 4)))
  (test (parse `{+ {* 3 4} 8})
        (opE-binary (add)
             (opE-binary (mul) (numE 3) (numE 4))
             (numE 8)))
  (test (parse `{if {= 0 1} {* 3 4} 8})
        (ifE (opE-binary (eql) (numE 0) (numE 1))
             (opE-binary (mul) (numE 3) (numE 4))
             (numE 8)))
   (test/exn (parse `{{+ 1 2}})
            "invalid input")
  (test/exn (parse `{+ 1})
            "unknown operator")
  (test/exn (parse `{^ 1 2})
            "unknown operator")
  (test (parse `{cond {{= 0 1} {* 3 4}}
                      {{= 1 1} 8}})
        (condE (list (pair (opE-binary (eql) (numE 0) (numE 1))
                           (opE-binary (mul) (numE 3) (numE 4)))
                     (pair (opE-binary (eql) (numE 1) (numE 1))
                           (numE 8))))))
  
;; eval --------------------------------------

(define-type Value
  (numV [n : Number])
  (boolV [b : Boolean]))

(define (op-bool-bool->proc [f : (Boolean -> Boolean)]) : (Value -> Value)
  (λ (v)
    (type-case Value v
      [(boolV n)
       (boolV (f n))]
      [else
          (error 'eval "type error")])))

(define (op-num-num->proc [f : (Number -> Number)]) : (Value -> Value)
  (λ (v)
    (type-case Value v
      [(numV n)
       (numV (f n))]
      [else
          (error 'eval "type error")])))

(define (op-num-num-num->proc [f : (Number Number -> Number)]) : (Value Value -> Value)
  (λ (v1 v2)
    (type-case Value v1
      [(numV n1)
       (type-case Value v2
         [(numV n2)
          (numV (f n1 n2))]
         [else
          (error 'eval "type error")])]
      [else
       (error 'eval "type error")])))

(define (op-num-num-bool->proc [f : (Number Number -> Boolean)]) : (Value Value -> Value)
  (λ (v1 v2)
    (type-case Value v1
      [(numV n1)
       (type-case Value v2
         [(numV n2)
          (boolV (f n1 n2))]
         [else
          (error 'eval "type error")])]
      [else
       (error 'eval "type error")])))

(define (op-bool-bool-bool->proc [f : (Boolean Boolean -> Boolean)]) : (Value Value -> Value)
  (λ (v1 v2)
    (type-case Value v1
      [(boolV n1)
       (type-case Value v2
         [(boolV n2)
          (boolV (f n1 n2))]
         [else
          (error 'eval "type error")])]
      [else
       (error 'eval "type error")])))

(define (op->proc-unary [op : Op-unary]) : (Value -> Value)
  (type-case Op-unary op
    [(neg) (op-bool-bool->proc not)]
    [(unm) (op-num-num->proc (λ (x) (* -1 x)))]))

(define (proc-and [p : Boolean] [q : Boolean]) : Boolean
  (and p q))

(define (proc-or [p : Boolean] [q : Boolean]) : Boolean
  (or p q))

(define (op->proc-binary [op : Op-binary]) : (Value Value -> Value)
  (type-case Op-binary op
    [(add) (op-num-num-num->proc +)]
    [(sub) (op-num-num-num->proc -)]
    [(mul) (op-num-num-num->proc *)]
    [(div) (op-num-num-num->proc /)]
    [(eql) (op-num-num-bool->proc =)]
    [(leq) (op-num-num-bool->proc <=)]
    [(cnj) (op-bool-bool-bool->proc proc-and)]
    [(dsj) (op-bool-bool-bool->proc proc-or)]))

(define (eval [e : Exp]) : Value
  (type-case Exp e
    [(numE n) (numV n)]
    [(opE-unary  o arg) ((op->proc-unary  o) (eval arg))]
    [(opE-binary o l r) ((op->proc-binary o) (eval l) (eval r))]
    [(ifE b l r)
     (type-case Value (eval b)
       [(boolV v)
        (if v (eval l) (eval r))]
       [else
        (error 'eval "type error")])]
    [(condE cs)
     (eval (cond->if cs))]))

(define (cond->if [cs : (Listof (Exp * Exp))]) : Exp
  (type-case (Listof (Exp * Exp)) cs
    [empty
     (numE 42)]
    [(cons c cs)
     (ifE (fst c)
          (snd c )
          (cond->if cs))]))

(define (run [e : S-Exp]) : Value
  (eval (parse e)))

(module+ test
  (test (run `2)
        (numV 2))
  (test (run `{+ 2 1})
        (numV 3))
  (test (run `{* 2 1})
        (numV 2))
  (test (run `{+ {* 2 3} {+ 5 8}})
        (numV 19))
  (test (run `{= 0 1})
        (boolV #f))
  (test (run `{if {= 0 1} {* 3 4} 8})
        (numV 8))
  (test (run `{cond {{= 0 1} {* 3 4}}
                    {{= 1 1} 8}})
        (numV 8))
  (test (run `{∧ {= 42 42} {<= -5 -4}})
        (boolV #t))
  (test (run `{∨ {~ {<= 3 14}} {<= {- 4} {* 2 3}}})
        (boolV #t)))

;; printer ———————————————————————————————————-

(define (value->string [v : Value]) : String
  (type-case Value v
    [(numV n) (to-string n)]
    [(boolV b) (if b "true" "false")]))

(define (print-value [v : Value]) : Void
  (display (value->string v)))

(define (main [e : S-Exp]) : Void
  (print-value (eval (parse e))))