#lang scheme/base

(define MAX_POPULATION 50)      ; Population size
(define MAX_GENERATION 500)      ; Generation to stop evolving at
(define MAX_PARENT_ITERATION 10) ; How many tries to find different parents
(define NUM_POPULATIONS 2)

(define MUTATION_PROBABILITY 0.05)
(define ELITISM_COUNT 2)

(provide solve)
(define (solve weights volumes costs W V C)
  
  ; Speciemen's fitness
  (define (fitness specimen)
    
    (define (remove-random-item specimen)
      
      (define (remove-item list pos)
        (if (= 0 pos)
            (cons (- 1 (car list)) (cdr list))
            (cons (car list) (remove-item (cdr list) (- pos 1)))))

      (let ((remove-pos (random (length specimen))))
        (if (= (list-ref specimen remove-pos) 1)
            (remove-item specimen remove-pos)
            (remove-random-item specimen))))
    
    (let ((st (stats specimen)))
      (if (or (and (<= (car st)  W)
                   (<= (cadr st) V))
              (= 0 (car st)))
          (caddr st)
          (fitness (remove-random-item specimen)))))
  
  ; Mutate this specimen
  (define (mutation specimen)
    (map (lambda (x)
           (if (<= (random) MUTATION_PROBABILITY)
               (- 1 x)
               x))
         specimen))
  
  ; Breed and create next population
  (define (evolve population generation)
    
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

    (define (take-best-specimens count)
      (define (loop sorted-pop head count)
        (if (= 0 count)
            head
            (loop (cdr sorted-pop) (cons (car sorted-pop) head) (- count 1))))
      (loop (sort population (lambda (x y) (> (fitness x) (fitness y)))) '() count))
    
    (define (new-generation likehoods new-population reset)
      
      (define (new-specimen iteration)
        
        (define (breed mother father)
          
          (define (list-head lst pos head)
            (if (= 0 pos)
                (reverse head)
                (list-head (cdr lst) (- pos 1) (cons (car lst) head))))
          
          (let* ((crossover-pos (random (+ (length father) 1)))
                 (child (mutation (append (list-head mother crossover-pos '()) (list-tail father crossover-pos))))
                 (child-fitness  (fitness child))
                 (mother-fitness (fitness mother))
                 (father-fitness (fitness father)))
            (cond ((and (< child-fitness (max mother-fitness father-fitness)) (< (random) 0.9)) father)
                  (else child))))
        
        (define (get-parent population likehoods likehood)
          (if (< likehood (car likehoods))
              (car population)
              (get-parent (cdr population) (cdr likehoods) likehood)))
        
        (let ((mother (get-parent population likehoods (random)))
              (father (get-parent population likehoods (random))))
          (if (or (not (equal? mother father)) (> iteration MAX_PARENT_ITERATION))
              (breed mother father)
              (new-specimen (+ iteration 1)))))
                    
      (cond (reset
             (append (take-best-specimens ELITISM_COUNT) (generate-first-population (- MAX_POPULATION ELITISM_COUNT))))
            ((= (length new-population) (- MAX_POPULATION ELITISM_COUNT))
             (append (take-best-specimens ELITISM_COUNT) new-population))
            (else
             (new-generation likehoods (cons (new-specimen 1) new-population) #f))))

    ; Check if most of the population is of the same fitness
    (define (check-dead-end fitness-map)
      (define (loop fitness-map cnt max min)
;        (cond ((and (null? fitness-map) (> (/ cnt MAX_POPULATION) 0.8)) (print (/ cnt MAX_POPULATION)) #t)
;              ((null? fitness-map) (if (= (+ generation 1) MAX_GENERATION) (print (/ cnt MAX_POPULATION)) (void)) #f)
;              ((< (- max (car fitness-map)) 4) (loop (cdr fitness-map) (+ cnt 1) max min))
;              (else (loop (cdr fitness-map) cnt max min))))
        (cond ((and (null? fitness-map) (> (/ cnt MAX_POPULATION) 0.8)) #t)
              ((null? fitness-map) #f)
              ((< (- max (car fitness-map)) 4)
               (loop (cdr fitness-map) (+ cnt 1) max min))
              (else
               (loop (cdr fitness-map) cnt max min))))

      (loop fitness-map 0 (apply max fitness-map) (apply min fitness-map)))

    (let* ((fitness-map (map fitness population))
           (total-fitness (foldl + 0 fitness-map)))
      (cond ((or (= MAX_GENERATION generation) (= 0 total-fitness))
             population)
            ((check-dead-end fitness-map)
             (evolve (new-generation (calculate-likehoods population fitness-map total-fitness) '() #t) (+ 1 generation)))
            (else
             (evolve (new-generation (calculate-likehoods population fitness-map total-fitness) '() #f) (+ 1 generation))))))
;      (if (or (= MAX_GENERATION generation)
;              (= 0 total-fitness)
;              (check-dead-end fitness-map))
;          population
;          ;(print population)
;          (evolve (new-generation (calculate-likehoods population fitness-map total-fitness) '()) (+ 1 generation)))))
  
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

  (define (evolution-loop population num-populations)
    (if (= num-populations 0)
        population
        (evolution-loop (evolve (generate-first-population (- MAX_POPULATION ELITISM_COUNT) 1) (- num-populations 1)))))
        
  ; Generate first ever population ant let them do their job
  (let ((best-specimen (find-best-specimen (evolve (generate-first-population MAX_POPULATION) 1))))
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