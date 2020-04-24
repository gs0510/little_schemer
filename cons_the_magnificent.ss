#lang racket

; Chapter 3: Cons the Magnificent!!

; have to define atom since it's not a primitive
(define (atom? x)
  (and (not (pair? x)) (not (null? x))))


; remove an element from a list

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((eq? (car lat) a) (cdr lat))
              (else (cons (car lat) (rember a (cdr lat)))))))))

; test!
(rember 'mint '(lamb chops and mint flavored jelly))
(rember 'toast '(bacon lettuce and tomato))
(rember 'cup '(coffee cup tea cup))


; firsts gets the first element of all the s-expressions inside a list
(define firsts
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      (else (cons (car (car lat)) (firsts (cdr lat)))))))
    
;test!
(firsts '((a b) (c d) (e f)))
(firsts '(((a) b) (c d) (e f)))
(firsts '())

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((eq? (car lat) old) (cons old (cons new (cdr lat))))
              (else (cons (car lat) (insertR new old (cdr lat)))))))))

;test!
(insertR 'topping 'fudge '(ice cream with fudge for dessert))
(insertR 'e 'd '(a b c d f g h))
(insertR 'topping 'fudge '(ice cream for dessert))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((eq? (car lat) old) (cons new lat))
              (else (cons (car lat) (insertL new old (cdr lat)))))))))

;test!
(insertL 'fudge 'topping  '(ice cream with topping for dessert))
(insertL 'd 'e '(a b c e f g h))
(insertL 'topping 'fudge '(ice cream for dessert))


(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((eq? (car lat) old) (cons new (cdr lat)))
              (else (cons (car lat) (subst new old (cdr lat)))))))))

;test!
(subst 'fudge 'topping  '(ice cream with topping for dessert))
(subst 'd 'e '(a b c e))
(subst 'topping 'fudge '(ice cream for dessert))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
              (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))))

;test!
(subst2 'fudge 'topping 'vanilla '(ice cream with vanilla topping for dessert))
(subst2 'd 'e 'a '(a b c e))
(subst2 'topping 'fudge 'ice '(ice cream for dessert))

; multirember removes all occurences of word a from the list
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((eq? (car lat) a) (multirember a (cdr lat)))
              (else (cons (car lat) (multirember a (cdr lat)))))))))

;test!
(multirember 'a '(a a a a a a))
(multirember 'a '())
(multirember 'a '(b a b a a b))

; multinsertR inserts 'new' to the right of all words 'old'
(define multinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((eq? (car lat) old) (cons (car lat) (cons new (multinsertR new old (cdr lat)))))
              (else (cons (car lat) (multinsertR new old (cdr lat)))))))))

;test!
(multinsertR 'a 'b '(b b b b b))
(multinsertR 'b 'a '(b b b b b))
(multinsertR 'a 'b '())

; multinsertL inserts 'new' to the left of all words 'old'
(define multinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((eq? (car lat) old) (cons new (cons old (multinsertL new old (cdr lat)))))
              (else (cons (car lat) (multinsertL new old (cdr lat)))))))))

;test!
(multinsertL 'a 'b '(b b b b b))
(multinsertL 'b 'a '(b b b b b))
(multinsertL 'a 'b '())
(multinsertL 'fried 'fish '(chips and fish or fish and fried))


(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
              (else (cons (car lat) (multisubst new old (cdr lat)))))))))

;test!
(multisubst 'fudge 'topping  '(ice cream with topping for topping))
(multisubst 'd 'e '(a e b c e))
(multisubst 'd 'e '())
(multisubst 'topping 'fudge '(ice cream for dessert))