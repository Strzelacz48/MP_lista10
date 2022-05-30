#lang plait

(module+ test
  (print-only-errors #t))

;; abstract syntax -------------------------------

(define-type Op
  (add)
  (sub)
  (mul)
  (div))

(define-type Exp
  (numE [n : Number])
  (opE [op : Op] [args : (Listof Exp)]))

;; parse ----------------------------------------

(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s)
     (numE (s-exp->number s))]
    [(s-exp-match? `{SYMBOL ANY ...} s)
     (opE (parse-op (s-exp->symbol (first (s-exp->list s))))
          (parse-op-args (rest (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(define (parse-op [op : Symbol]) : Op
  (cond
    [(eq? op '+) (add)]
    [(eq? op '-) (sub)]
    [(eq? op '*) (mul)]
    [(eq? op '/) (div)]
    [else (error 'parse "unknown operator")]))

(define (parse-op-args [xs : (Listof S-Exp)]) : (Listof Exp)
  (type-case (Listof S-Exp) xs
    [empty empty]
    [(cons elem tail) (cons (parse elem) 
                            (parse-op-args tail))]))
                 
(module+ test
  (test (parse `2)
        (numE 2))
  (test (parse `{+ 2 1})
        (opE (add) (list (numE 2) (numE 1))))
  (test (parse `{* 3 4})
        (opE (mul) (list (numE 3) (numE 4))))
  (test (parse `{+ {* 3 4} 8})
        (opE (add)
             (list (opE (mul) (list (numE 3) (numE 4)))
             (numE 8))))
  (test/exn (parse `{{+ 1 2}})
            "invalid input")
  (test (parse `{+ 1})
        (opE (add) (list (numE 1))))
  (test/exn (parse `{^ 1 2})
            "unknown operator"))
  
;; eval --------------------------------------

(define-type-alias Value Number)

(define (my-subtraction [x : Number] [y : Number]) : Number
  (- y x))

(define (my-division [x : Number] [y : Number]) : Number
  (/ y x))

(define (op->proc [op : Op]) : ((Listof Value) -> Value)
  (type-case Op op
    [(add) (lambda (xs) (foldr + 0 xs))]
    [(sub) (lambda (xs) (if (empty? xs)
                            0
                            (foldr my-subtraction (first xs) (rest xs))))]
    [(mul) (lambda (xs) (if (empty? xs)
                            0
                            (foldr * 1 xs)))]
    [(div) (lambda (xs) (if (empty? xs)
                            0
                            (foldr my-division (first xs) (rest xs))))]))

(define (eval [e : Exp]) : Value
  (type-case Exp e
    [(numE n) n]
    [(opE o xs) ((op->proc o) (eval-op-args xs))]))

(define (eval-op-args [xs : (Listof Exp)]) : (Listof Number)
  (type-case (Listof Exp) xs
    [empty empty]
    [(cons elem tail) (cons (eval elem)
                            (eval-op-args tail))]))

(define (run [e : S-Exp]) : Value
  (eval (parse e)))

(module+ test
  (test (run `2)
        2)
  (test (run `{+ 2 1})
        3)
  (test (run `{* 2 1})
        2)
  (test (run `{+ {* 2 3} {+ 5 8}})
        19)
  (test (run `{+})
        0)
  (test (run `{* 1})
        1)
  (test (run `{+ 1 2 3 4 5 6 7 8 9 10})
        55)
  (test (run `{* 1 {+ 1 2} {* 1 2 3}})
        18))

;; printer ———————————————————————————————————-

(define (print-value [v : Value]) : Void
  (display v))

(define (main [e : S-Exp]) : Void
  (print-value (eval (parse e))))