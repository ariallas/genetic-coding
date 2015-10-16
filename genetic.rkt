#lang scheme/base

(define MAX_POPULATION 30) ; Population size
(define MAX_GENERATION 100) ; Generation to stop evolving at
(define MAX_PARENT_ITERATION 10) 

(define COST_COEF 4)
(define BAD_COEF -1)
(define BOUND_MULTIPLIER 3)

(provide solve)
(define (solve weights volumes costs W V C)

  ; Speciemen's fitness
  (define (fitness specimen)
    (let ((st (stats specimen))
          (total-weight (foldl + 0 weights))
          (total-volume (foldl + 0 volumes))
          (total-cost   (foldl + 0 costs)))
      (if (or (> (car st) W) (> (cadr st) V) (= (caddr st) 0))
          1
          (* (/ (caddr st) total-cost) 100)))) 
      ;(+ (* (car st)   BAD_COEF (if (> (car st)  W) BOUND_MULTIPLIER 1))
      ;   (* (cadr st)  BAD_COEF (if (> (cadr st) V) BOUND_MULTIPLIER 1))
      ;   (* (caddr st) COST_COEF)
      ;   (* (- BAD_COEF) BOUND_MULTIPLIER (+ (- total-weight W))))))

  ; Mutate this specimen
  (define (mutation specimen)
    (define (invert-pos list pos)
      (if (= 0 pos)
          (cons (- 1 (car list)) (cdr list))
          (cons (car list) (invert-pos (cdr list) (- pos 1)))))
    (invert-pos specimen (random (length specimen))))
  
  ; Breed and create next population
  (define (evolve population generation best-specimen)

    (define (calculate-likehoods population)
      
      (define (loop population likehoods total-fitness)
        
        (define (calculate-likehood specimen last-likehood)
          (+ last-likehood (/ (fitness specimen) total-fitness)))
        
        (if (null? population)
            (reverse likehoods)
            (loop (cdr population)
                  (cons (calculate-likehood (car population) (if (null? likehoods)
                                                                 0
                                                                 (car likehoods)))
                        likehoods)
                  total-fitness)))

      (loop population '() (foldl + 0 (map fitness population))))

    ; Find the best specimen that fits the conditions
    (define (find-best-specimen current-best)
      (foldl (lambda (a b)
               (let ((stats-a (stats a))
                     (stats-b (stats b)))
                 (if (and (<= (car stats-a) W) (<= (cadr stats-a) V) (>= (caddr stats-a) C) (or (null? b) (>= (caddr stats-a) (caddr stats-b))))
                     a
                     b)))
             current-best
             population))
    
    (define (new-generation likehoods new-population)

      (define (new-specimen iteration)

        (define (breed mother father)
          
          (define (list-head lst pos head)
            (if (= 0 pos)
                (reverse head)
                (list-head (cdr lst) (- pos 1) (cons (car lst) head))))
          
          (let* ((crossover-pos (random (+ (length father) 1)))
                 (child (append (list-head mother crossover-pos '()) (list-tail father crossover-pos))))
            (if (< 0.2 (random))
                (mutation child)
                child)))
          
        (define (get-parent population likehoods likehood)
          (if (< likehood (car likehoods))
              (car population)
              (get-parent (cdr population) (cdr likehoods) likehood)))
                
        (let ((mother (get-parent population likehoods (random)))
              (father (get-parent population likehoods (random))))
          (if (or (not (equal? mother father)) (> iteration MAX_PARENT_ITERATION))
              (breed mother father)
              (new-specimen (+ iteration 1)))))
      
      (if (= (length new-population) MAX_POPULATION)
          new-population
          (new-generation likehoods (cons (new-specimen 1) new-population))))

    (if (= MAX_GENERATION generation)
        (find-best-specimen best-specimen)
        (evolve (new-generation (calculate-likehoods population) '()) (+ 1 generation) (find-best-specimen best-specimen))))
  
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
;  (let ((best-specimen (find-best-specimen (evolve (generate-first-population MAX_POPULATION) 1))))
;    (if (null? best-specimen)
;        '(#f)
;        (cons #t (append (stats best-specimen) (list (convert-specimen best-specimen)))))))

  (let ((best-specimen (evolve (generate-first-population MAX_POPULATION) 1 '())))
    (if (null? best-specimen)
        '(#f)
        (cons #t (append (stats best-specimen) (list (convert-specimen best-specimen)))))))
;  (let ((pop (generate-first-population MAX_POPULATION)))
;    (for-each (lambda (x)
;                (newline)
;                (print (fitness x))
;                (print (stats x))
;                (print x))
;              pop)
;    (newline)
;    (newline)
;    (for-each (lambda (x)
;                (newline)
;                (print (fitness x))
;                (print (stats x))
;                (print x))
;              (evolve pop 1))))