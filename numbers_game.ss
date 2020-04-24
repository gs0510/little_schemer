#lang racket
7
; have to define atom since it's not a primitive
(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define add1
  (lambda (n) (+ n 1)))

(define sub1
  (lambda (n) (- n 1)))

;test
(sub1 0)

(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))

;test
(o+ 22 12)

(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))

;test
(o- 22 12)
(o- 12 22)

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup) (addtup (cdr tup)))))))

;test
(addtup '(1 3 5 7))
(addtup '(0 2 3 5))

(define *
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (* n (sub1 m)))))))

;test
(* 5 0)
(* 5 2)


(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (o+ (car tup1) (car tup2))
            (tup+ (cdr tup1) (cdr tup2)))))))

(tup+ '(1 2) '(3 4 5))
(tup+ '() '(1 2 3))
(tup+ '(1 2) '())
(tup+ '(1 2 3) '(1 2 3))

(define >
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (> (sub1 n) (sub1 m))))))

(> 5 5)
(> 5 6)
(> 6 5)

(define <
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (< (sub1 n) (sub1 m))))))

(< 5 5)
(< 5 6)
(< 6 5)

(define expt
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (* n (expt n (sub1 m)))))))

(expt 2 3)
(expt 1 1)
(expt 5 2)

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

(length '(a b c d))
(length '())


(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(pick 4 '(a b c d e))
(pick 5 '(a b c d e))
(pick 1 '(a b))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(rempick 4 '(a b c d e))
(rempick 5 '(a b c d e))
(rempick 1 '(a b))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else
    (cond
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))))

(no-nums '(a b 5 c 5 d))
(no-nums '(a b C))
(no-nums '())

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
              (else (all-nums (cdr lat))))))))

(all-nums '(a b c))
(all-nums '(a 5 c 6 d 2))

(define eqan?
  (lambda (one two)
    (cond
      ((and (number? one) (number? two) (= one two)))
      ((or (number? one) (number? two) #f))
      (else (eq? one two)))))

(eqan? 1 3)
(eqan? 1 1)
(eqan? 1 '(1))
(eqan? '(1) '(1))
      
(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else (cond
              ((eq? (car lat) a) (add1 (occur a (cdr lat))))
              (else (occur a (cdr lat))))))))

(occur 2 '(a 2 2 2 2 2 a b))
(occur 2 '())
(occur 2 '(a b c 3 d))