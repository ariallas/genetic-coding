#lang scheme/base

(define MAX_POPULATION 30) ; Population size
(define MAX_GENERATION 50) ; Generation to stop evolving at

(provide solve)
(define (solve weights volumes costs W V C)
  
  ; Breed and create next population
  (define (new-population population generation)
    '())
  
  ; Be a god
  (define (generate-first-population N)
    ; Generate random specimen
    (define (generate-specimen)
      (build-list (length weights) (lambda (x) (random 2))))
    ; Internal loop
    (define (loop N population)
      (if (= 0 N)
          population
          (loop (- N 1) (cons (generate-specimen) population))))
    (loop N '()))
  
  ; Find the best specimen that fits the conditions
  (define (find-best-specimen population)
    (foldl (lambda (a b)
             (let ((stats-a (stats a))
                   (stats-b (stats b)))
               (if (and (<= (car stats-a) W) (<= (cadr stats-a) V) (>= (caddr stats-a) C) (or (null? b) (>= (caddr stats-a) (caddr stats-b))))
                   a
                   b)))
           '()
           population))

  ; Calculate specimen stats
  (define (stats specimen)
    (define (loop specimen weights volumes costs w v c)
      (cond ((null? specimen)
             (list w v c))
            ((= 1 (car specimen))
             (loop (cdr specimen) (cdr weights) (cdr volumes) (cdr costs) (+ w (car weights)) (+ v (car volumes)) (+ c (car costs))))
            (else
             (loop (cdr specimen) (cdr weights) (cdr volumes) (cdr costs) w v c))))
    (loop specimen weights volumes costs 0 0 0))

  ; Convert specimen from bit representation to number list
  (define (convert-specimen specimen)
    ; Internal loop
    (define (loop specimen N result)
      (cond ((null? specimen)
             result)
            ((= 1 (car specimen))
             (loop (cdr specimen) (+ N 1) (append result (list N))))
            (else
             (loop (cdr specimen) (+ N 1) result))))
    (loop specimen 1 '()))

  ; Generate first ever population ant let them do their job
  ;(let ((best-specimen (find-best-specimen (new-population (generate-first-population MAX_POPULATION)))))
  ;  (if (null? best-specimen)
  ;      '(#f)
  ;      (cons #t (convert-specimen best-specimen)))))

  ; For testing
  (let ((best-specimen (find-best-specimen (generate-first-population MAX_POPULATION))))
   (if (null? best-specimen)
       '(#f)
       (cons #t (append (stats best-specimen) (list (convert-specimen best-specimen)))))))