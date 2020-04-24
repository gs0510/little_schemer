#lang racket

(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define rember-f
  (lambda (test? a l)
    (cond
      ((null? l) (quote ()))
      ((test? (car l) a) (cdr l))
      (else (cons (car l) (rember-f test? a (cdr l)))))))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define eq?-salad (eq?-c 'salad))

;test
(eq?-salad 'salad)

;writing rember-f as a function of one
(define rember-f1
  (lambda (test?)
  (lambda (a l)
    (cond
      ((null? l) (quote ()))
      ((test? (car l) a) (cdr l))
      (else (cons (car l) (rember-f1 test? a (cdr l))))))))

(define rember-eq? (rember-f1 eq?))

;test
(rember-eq? 'tuna '(tuna salad is good))

(define consR
  (lambda (new old l)
    (cons old (cons new l))))

(define consL
  (lambda (new old l)
    (cons new (cons old l))))

(define insert-g
  (lambda (position)
    (lambda (new old l)
      (cond
        ((null? l) (quote ()))
        ((eq? (car l) old) (position new old (cdr l)))
        (else (cons (car l) ((insert-g position) new old (cdr l))))))))

(define insert-R (insert-g consR))
(define insert-L (insert-g consL))

;test
(insert-R 'c 'e '(a b c d))
