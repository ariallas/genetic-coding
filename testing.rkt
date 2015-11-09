#lang scheme/base

(require rackunit)
(require "genetic.rkt")

; Total number of tests
(define cnt 0)
; Number of succeded tests
(define succeded 0)

; Just checking all tests...
(let ((tests (with-input-from-file "tests.txt" read)))

  (define (check-anwsers result anwsers anwsers-original)
    (cond ((null? anwsers)
           (fprintf (current-output-port) "~nTest ~a failed. Got:~n~a~nExpected:~n~a~n" cnt result (car anwsers-original)))
          ((equal? result (car anwsers))
           (set! succeded (+ succeded 1)))
          (else
           (check-anwsers result (cdr anwsers) anwsers-original))))

  (for-each
   (lambda (test)
     (set! cnt (+ cnt 1))
     (check-anwsers (apply solve (car test)) (cadr test) (cadr test)))
   tests))

(newline)
(fprintf (current-output-port) "~n~a out of ~a tests succeded~n" succeded cnt)