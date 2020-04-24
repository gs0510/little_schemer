#lang racket

(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

  (define fact
    ((lambda (procedure)
       (lambda (n)
         (if (zero? n)
             1
             (* n ((procedure procedure) (- n 1))))))
     (lambda (procedure)
       (lambda (n)
         (if (zero? n)
             1
             (* n ((procedure procedure) (- n 1))))))))

(fact 6)

  (((lambda (procedure)
      (lambda (n)
        (if (zero? n)
            1
            (* n ((procedure procedure) (- n 1))))))
    (lambda (procedure)
      (lambda (n)
        (if (zero? n)
            1
            (* n ((procedure procedure) (- n 1)))))))
   5)