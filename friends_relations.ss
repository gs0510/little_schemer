#lang racket

(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a) (member? a (cdr lat)))))))

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

;test
(set? '(a b c a))
(set? '())
(set? '(a b c))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))

;test
(makeset '(a b a c c b))

(define subset?
  (lambda (s1 s2)
    (cond
      ((null? s1) #t)
      (else (cond
             ((member? (car s1) s2) (subset? (cdr s1) s2))
             (else #f))))))
;test
(subset? '(a b c) '(d b a c))
(subset? '() '(a b))
(subset? '(a) '(b c))

(define eqset?
  (lambda (s1 s2)
    (and (subset? s1 s2) (subset? s2 s1))))

;test
(eqset? '(a b) '(b a))
(eqset? '() '())
(eqset? '(a b c) '(a b))

(define intersect?
  (lambda (s1 s2)
    (cond
      ((null? s1) #f)
      ((member? (car s1) s2) #t)
      (else (intersect? (cdr s1) s2)))))

;test
(intersect? '(a b) '(b))
(intersect? '(b c) '(a))
(intersect? '() '())

(define intersect
  (lambda (s1 s2)
    (cond
      ((null? s1) (quote ()))
      ((member? (car s1) s2) (cons (car s1) (intersect (cdr s1) s2)))
      (else (intersect (cdr s1) s2)))))

;test
(intersect '(a b) '(b a x))
(intersect '(b c) '(a))
(intersect '() '())

(define union
  (lambda (s1 s2)
    (cond
      ((null? s1) s2)
      ((member? (car s1) s2) (union (cdr s1) s2))
      (else (cons (car s1) (union (cdr s1) s2))))))

;test
(union '(a b c) '(c b d))
(union '() '(c b d))
(union '(g f h) '(c b d))

