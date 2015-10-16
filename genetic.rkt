#lang scheme/base

(define MAX_POPULATION 30) ; Population size
(define MAX_GENERATION 50) ; Generation to stop evolving at

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
      (if (or (> (car st) W) (> (cadr st) V))
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
  (define (evolve population generation)

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
    
    (define (new-generation likehoods new-population)

      (define (new-specimen)
        '())
      
      (if (= (length new-population) MAX_POPULATION)
          new-population
          (new-generation likehoods (cons (new-specimen) new-population))))

    ;(if (= MAX_GENERATION generation)
    ;    population
    ;    (evolve (new-generation population (calculate-likehoods population) '()) (+ 1 generation))))
    (calculate-likehoods population))
  
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
  ;(let ((best-specimen (find-best-specimen (evolve (generate-first-population MAX_POPULATION) 1))))
  ;  (if (null? best-specimen)
  ;      '(#f)
  ;      (cons #t (convert-specimen best-specimen)))))

  (let ((pop (generate-first-population 10)))
    (for-each (lambda (x)
                (newline)
                (print (fitness x))
                (print (stats x))
                (print x))
              pop)
    (newline)
    (print (evolve pop 1))))