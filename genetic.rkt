#lang racket/gui

(define MAX_POPULATION 50)      ; Population size
(define MAX_GENERATION 400)      ; Generation to stop evolving at
(define MAX_PARENT_ITERATION 10) ; How many tries to find different parents
(define NUM_POPULATIONS 15)

(define MUTATION_PROBABILITY 0.05)
(define ELITISM_COUNT 2)

(provide solve)
(define (solve weights volumes costs W V C)
  
  ; Calculate speciemen's fitness
  (define (fitness specimen)
    ; Remove one random item from this specimen
    (define (remove-random-item specimen)
      ; Remove item number num
      (define (remove-item list num)
        (if (= 0 num)
            (cons (- 1 (car list)) (cdr list))
            (cons (car list) (remove-item (cdr list) (- num 1)))))
      
      (let ((remove-pos (random (length specimen))))
        (if (= (list-ref specimen remove-pos) 1)
            (remove-item specimen remove-pos)
            (remove-random-item specimen))))
    
    ; Return price of the specimen if conditions are met, otherwise remove random item and check again
    (let ((st (stats specimen)))
      (if (or (and (<= (car st)  W)
                   (<= (cadr st) V))
              (= 0 (car st)))
          (caddr st)
          (fitness (remove-random-item specimen)))))
  
  ; Mutate this specimen (invert genes with low probability)
  (define (mutation specimen)
    (map (lambda (x)
           (if (<= (random) MUTATION_PROBABILITY)
               (- 1 x)
               x))
         specimen))
  
  ; Breed and create next population
  (define (evolve population generation num-population)
    ; Calculate list of likehoods for roulette-wheel selection 
    (define (calculate-likehoods population fitness-map total-fitness)
      (define (loop population likehoods fitness-map)
        
        (define (calculate-likehood specimen-fitness last-likehood)
          (+ last-likehood (/ specimen-fitness total-fitness)))
        
        (if (null? population)
            (reverse likehoods)
            (loop (cdr population)
                  (cons (calculate-likehood (car fitness-map)
                                            (if (null? likehoods)
                                                0
                                                (car likehoods)))
                        likehoods)
                  (cdr fitness-map))))
      
      (loop population '() fitness-map))
    
    ; Return list of count specimen with best fitness from the current population
    (define (take-best-specimens count)
      (define (loop sorted-pop head count)
        (if (= 0 count)
            head
            (loop (cdr sorted-pop) (cons (car sorted-pop) head) (- count 1))))
      (loop (sort population (lambda (x y) (> (fitness x) (fitness y)))) '() count))
    
    ; Generate new population from the current
    (define (new-generation likehoods new-population reset)
      ; Choose parents and create one new specimen from them
      (define (new-specimen iteration)
        ; Create new specimen from mother and father
        (define (breed mother father)
          ; Return list of first pos elements of the lst
          (define (list-head lst pos head)
            (if (= 0 pos)
                (reverse head)
                (list-head (cdr lst) (- pos 1) (cons (car lst) head))))
          
          ; Breed, but take one of the parents if the child is too weak
          (let* ((crossover-pos (random (+ (length father) 1)))
                 (child (mutation (append (list-head mother crossover-pos '()) (list-tail father crossover-pos))))
                 (child-fitness  (fitness child))
                 (mother-fitness (fitness mother))
                 (father-fitness (fitness father)))
            (cond ((and (< child-fitness (max mother-fitness father-fitness)) (< (random) 0.9)) father)
                  (else child))))
        
        ; Get parent by the likehoods
        (define (get-parent population likehoods likehood)
          (if (< likehood (car likehoods))
              (car population)
              (get-parent (cdr population) (cdr likehoods) likehood)))
        
        ; Choose random parents by roulette-wheel selection, try a few times if parents are the same
        (let ((mother (get-parent population likehoods (random)))
              (father (get-parent population likehoods (random))))
          (if (or (not (equal? mother father)) (> iteration MAX_PARENT_ITERATION))
              (breed mother father)
              (new-specimen (+ iteration 1)))))
      
      ; Generate a new population from an old one, or take a few of the best and restart, if reset flag is true
      (cond (reset
             (append (take-best-specimens ELITISM_COUNT) (generate-first-population (- MAX_POPULATION ELITISM_COUNT))))
            ((= (length new-population) (- MAX_POPULATION ELITISM_COUNT))
             (append (take-best-specimens ELITISM_COUNT) new-population))
            (else
             (new-generation likehoods (cons (new-specimen 1) new-population) #f))))
    
    ; Check if most of the population is of the similar fitness
    (define (check-dead-end fitness-map)
      (define (loop fitness-map cnt max delta)
        (cond ((and (null? fitness-map) (> (/ cnt MAX_POPULATION) 0.8))
               #t)
              ((null? fitness-map)
               #f)
              ((< (- max (car fitness-map)) delta)
               (loop (cdr fitness-map) (+ cnt 1) max delta))
              (else
               (loop (cdr fitness-map) cnt max delta))))
      
      (loop fitness-map 0 (apply max fitness-map) (apply max costs)))
    
    ; Do the evolution!
    (let* ((fitness-map (map fitness population))
           (total-fitness (foldl + 0 fitness-map)))
      (cond ((or (= MAX_GENERATION generation) (= 0 total-fitness) (> num-population NUM_POPULATIONS))
             population)
            ((check-dead-end fitness-map)
             (evolve (new-generation (calculate-likehoods population fitness-map total-fitness) '() #t) (+ 1 generation) (+ 1 num-population)))
            (else
             (evolve (new-generation (calculate-likehoods population fitness-map total-fitness) '() #f) (+ 1 generation) num-population)))))
  
  ; Generate random population
  (define (generate-first-population N)
    ; Generate random specimen
    (define (generate-specimen)
      (build-list (length weights) (lambda (x) (random 2))))
    
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
  
  ; Do some gui stuff
  (define frame (new frame% [label "Genetic"] [width 300] [height 300]))
  (define msg (new message% [parent frame]
                   [label "Calculating solution..."]))
  
  
  ; Generate first ever population ant let them do their job
  (send frame show #t)
  (send msg set-label "Calculating solution...")
  (let* ((best-specimen (find-best-specimen (evolve (generate-first-population MAX_POPULATION) 1 1)))
         (result (if (null? best-specimen)
                     '(#f)
                     (cons #t (append (stats best-specimen) (list (convert-specimen best-specimen)))))))
    (if (eq? #f (car result))
        (send msg set-label "No solution")
        (send msg set-label "Solution found"))
    result))
