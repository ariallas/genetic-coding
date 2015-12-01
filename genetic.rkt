#lang racket/gui
(require plot)
; (solve '(4 7 3 2 2 6 3 5 5 8 10 8 2 8 5 9 4) '(7 5 7 9 1 8 4 4 1 7 4 6 6 2 4 2 10) '(7 10 7 3 1 7 5 8 4 10 1 10 9 3 4 3 8) 29 20 33)

(define MAX_POPULATION 50)       ; Population size
(define MAX_GENERATION 400)      ; Generation to stop evolving at
(define MAX_PARENT_ITERATION 10) ; How many tries to find different parents
(define NUM_POPULATIONS 10)

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
  (define (evolve population generation num-population process-data)
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
    
    (define (add-process-data process-data fitness-map)
      (cons (list (apply max fitness-map) (apply min fitness-map) (/ (foldl + 0 fitness-map) (length fitness-map))) process-data ))
    
    ; Do the evolution!
    (let* ((fitness-map (map fitness population))
           (total-fitness (foldl + 0 fitness-map)))
      (cond ((or (= MAX_GENERATION generation) (= 0 total-fitness) (> num-population NUM_POPULATIONS))
             (cons population (reverse process-data)))
            ((check-dead-end fitness-map)
             (evolve (new-generation (calculate-likehoods population fitness-map total-fitness) '() #t) (+ 1 generation) (+ 1 num-population) (add-process-data process-data fitness-map)))
            (else
             (evolve (new-generation (calculate-likehoods population fitness-map total-fitness) '() #f) (+ 1 generation) num-population (add-process-data process-data fitness-map))))))
  
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
  
  (define (draw-percent-bar frame label max_value given_value)
    (define panel (new horizontal-panel% [parent frame] [min-height 24] [stretchable-height #f] [horiz-margin 10]))
    (new message% [parent panel] [label label] [min-width 60])
    (new canvas%
         [parent panel]
         [style '(transparent)]
         [paint-callback
          (lambda (canvas dc)
            (send dc set-brush "pink" 'solid)
            (send dc draw-rectangle 5 5 200 15)
            (send dc set-brush "palegreen" 'solid)
            (send dc draw-rectangle 7 7 (* (/ given_value max_value) 194) 11)
            )]))
  
  ;  (define (draw-process process-data)
  ;    (define c (new editor-canvas% [parent frame]))
  ;    (define pb (new pasteboard%))
  ;    (send c set-editor pb)
  ;    (define ys process-data)
  ;    (define xs (build-list (length process-data) (lambda (x) (+ x 1))))
  ;    (print (length process-data))
  ;    (send pb insert (plot-snip (lines (map vector xs ys) #:color 'red) #:y-min 0 #:x-label "Iteration" #:y-label "Fitness")))
  
  (define (draw-graph dots y-max title x-label y-label)
    (define panel (new horizontal-panel% [parent process-panel]
                       [alignment '(center center)]
                       [min-height 210] [stretchable-height #f]
                       [min-width 606] [stretchable-width #f]
                       [vert-margin 5] [horiz-margin 10]
                       [border 0]
                       [style '(border)]))
    (define ys dots)
    (define xs (build-list (length dots) (lambda (x) (+ x 1))))
    (new canvas%
         [parent panel]
         [paint-callback
          (lambda (canvas dc)
            (plot/dc (lines (map vector xs ys) #:color 'red) dc 0 0 600 200 #:y-min 0 #:y-max y-max #:x-label x-label #:y-label y-label #:title title))]))
  
  (define (draw-items items)
    (define (filter-item-params params items filtered-params cnt)
      (if (null? items)
          (reverse filtered-params)
          (if (= cnt (car items))
              (filter-item-params (cdr params) (cdr items) (cons (car params) filtered-params) (+ cnt 1))
              (filter-item-params (cdr params) items filtered-params (+ cnt 1)))))
    
    (define (draw-histogram labels weights volumes costs title) 
      (define panel (new horizontal-panel% [parent items-panel]
                         [alignment '(center center)]
                         [min-height 260] [stretchable-height #f]
                         [min-width 606] [stretchable-width #f]
                         [vert-margin 5] [horiz-margin 10]
                         [border 0]
                         [style '(border)]))
      (new canvas%
           [parent panel]
           [paint-callback
            (lambda (canvas dc)
              (plot/dc (list (discrete-histogram (map vector labels weights) #:skip 4.5 #:x-min 0   #:color 1 #:label "Weight")
                             (discrete-histogram (map vector labels volumes) #:skip 4.5 #:x-min 1.2 #:color 3 #:label "Volume")
                             (discrete-histogram (map vector labels costs)   #:skip 4.5 #:x-min 2.4 #:color 2 #:label "Cost"))
                       dc 0 0 600 250 #:x-label "Item" #:title title))]))
    
    (draw-histogram (build-list (length weights) (lambda (x) (+ x 1))) weights volumes costs "All items")
    (draw-histogram items (filter-item-params weights items '() 1) (filter-item-params volumes items '() 1) (filter-item-params costs items '() 1) "Taken items"))
  
  (define (draw-process process-data)
    (define y-max (+ (apply max (map car process-data)) 1))
    (draw-graph (map car process-data)   y-max "Max fitness"     "Iteration" "Fitness")
    (draw-graph (map caddr process-data) y-max "Average fitness" "Iteration" "Fitness")
    (draw-graph (map cadr process-data)  y-max "Min fitness"     "Iteration" "Fitness"))
  
  (define (draw-anwser weight volume cost items)
    (draw-percent-bar items-panel "Weight: " W weight)
    (draw-percent-bar items-panel "Volume: " V volume)
    (draw-percent-bar items-panel "Cost: "   cost C))
  
  (define (draw-result weight volume cost items process-data)
    (send msg set-label (format "Solution found.  Weight: ~a  Volume: ~a  Cost: ~a  Items: ~a" weight volume cost items))
    (draw-items items)
    (draw-anwser weight volume cost items)
    (draw-process process-data)
    )
  
  ; Do some gui stuff
  (define main-frame (new frame% [label "Genetic"]))
  (define msg (new message% [parent main-frame] [label "Calculating solution..."] [auto-resize #t]))
  (define horizontal-frame (new horizontal-panel% [parent main-frame]))
  (define process-panel (new vertical-panel% [parent horizontal-frame]))
  (define items-panel   (new vertical-panel% [parent horizontal-frame]))  
  
  ; Generate first ever population ant let them do their job
  (send main-frame show #t)
  (send msg set-label "Calculating solution...")
  (let* ((population-data (evolve (generate-first-population MAX_POPULATION) 1 1 '()))
         (best-specimen (find-best-specimen (car population-data)))
         (result (if (null? best-specimen)
                     '(#f)
                     (cons #t (append (stats best-specimen) (list (convert-specimen best-specimen)))))))
    (if (eq? #f (car result))
        (send msg set-label "No solution")
        (draw-result (cadr result) (caddr result) (cadddr result) (cadddr (cdr result)) (cdr population-data)))
    result))
