#lang scheme/base

(require rackunit)
(require "genetic.rkt")

; Total number of tests
(define cnt 0)
; Number of succeded tests
(define succeded 0)

; Just checking all tests...
(let ((tests (with-input-from-file "tests.txt" read)))
  (for-each
   (lambda (test)
     (set! cnt (+ cnt 1))
     (test-begin
      (check-equal? (apply solve (car test)) (cadr test) (car test))
      (set! succeded (+ succeded 1))))
   tests))
(newline)
(fprintf (current-output-port) "~n~a out of ~a tests succeded~n" succeded cnt)