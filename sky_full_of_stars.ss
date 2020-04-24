#lang racket

(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

; rember* removes a from a list l looking into all the s-expressions
(define rember*
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((atom? (car lat))
        (cond
          ((eq? (car lat) a) (rember* a (cdr lat)))
          (else (cons (car lat) (rember* a (cdr lat))))))
      (else (cons (rember* a (car lat)) (rember* a (cdr lat)))))))

;test
(rember* 'cup '((coffee) cup ((tea) cup) (and (hic)) cup))

; insertR*
(define insertR*
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((atom? (car lat))
       (cond
         ((eq? (car lat) old) (cons (car lat) (cons new (cdr lat))))
         (else (cons (car lat) (insertR* new old (cdr lat))))))
      (else (cons (insertR* new old (car lat)) (insertR* new old (cdr lat)))))))

; test0
(insertR* 'roast 'chuck '((how much (wood)) could ((a (wood chuck))) (((chuck))) (if (a) ((wood chuck))) could chuck wood))

(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))

(define add1
  (lambda (n) (+ n 1)))

; occur*
(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? (car l) a) (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))
         ))
       (else (o+ (occur* a (car l)) (occur* a (cdr l)))))))

;test
(occur* 'banana '((banana) (((banana ice)) (cream (banana))) (banana) (banana brandy)))

; subst*
(define subst*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons new (subst* new old (cdr l))))
         (else (cons (car l) (subst* new old (cdr l))))))
       (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(subst* 'orange 'banana '((banana) (((banana ice)) (cream (banana))) (banana) (banana brandy)))


(define eqan?
  (lambda (one two)
    (cond
      ((and (number? one) (number? two) (= one two)))
      ((or (number? one) (number? two) #f))
      (else (eq? one two)))))
