#lang scheme/base

(require rackunit)
(require "genetic.rkt")

; Total number of tests
(define cnt 0)
; Number of succeded tests
(define succeded 0)

; Just checking all tests...
(let ((tests (with-input-from-file "tests.txt" read)))

  (define (check-anwsers result anwsers)
    (cond ((null? anwsers)
           (print 'Toobad))
          ((equal? result (car anwsers))
           (print 'Nice)
           (set! succeded (+ succeded 1)))
          (else
           (check-anwsers result (cdr anwsers)))))

  (for-each
   (lambda (test)
     (set! cnt (+ cnt 1))
     (check-anwsers (apply solve (car test)) (cdr test)))
   tests))

(newline)
(fprintf (current-output-port) "~n~a out of ~a tests succeded~n" succeded cnt)