#lang scheme/base

(require rackunit)
(require "genetic.rkt")

; Total number of tests
(define cnt 0)
; Number of succeded tests
(define succeded 0)

; Just checking all tests...
(let ((tests (with-input-from-file "tests.txt" read)))

  (define (check-anwsers result anwsers anwsers-original test)
    (cond ((null? anwsers)
           (fprintf (current-output-port) "~nTest ~a failed. Got:~n~a~nExpected:~n~a~nTest:~n~a~n" cnt result (car anwsers-original) test))
          ((equal? result (car anwsers))
           (if (= (modulo cnt 10) 0) (fprintf (current-output-port) "~a tests done~n" cnt) (void))
           (set! succeded (+ succeded 1)))
          (else
           (check-anwsers result (cdr anwsers) anwsers-original test))))

  (for-each
   (lambda (test)
     (set! cnt (+ cnt 1))
     (check-anwsers (apply solve (car test)) (cadr test) (cadr test) (car test)))
   tests))

(newline)
(fprintf (current-output-port) "~n~a out of ~a tests succeded~n" succeded cnt)