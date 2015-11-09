#lang scheme/base

(define TEST_SIZE 5)
(define MAX_ITEM_PARAM 10)

; Just random + 1
(define (natural-random r)
  (+ (random r) 1))

; Generate tests and write them to file
(define (generate-tests-file num-tests)
  
  ; Test generator
  (define (generate-tests num-tests tests)

    ; Generate a list of N random items from 1 to MAX_ITEM_PARAM
    (define (generate-items N)
      (if (= N 0)
          '()
          (cons (natural-random MAX_ITEM_PARAM) (generate-items (- N 1)))))

    ; Generate a random test
    (define (generate-test)
      
      (let ((N (natural-random TEST_SIZE))
            (W (natural-random (truncate (/ (* MAX_ITEM_PARAM TEST_SIZE) 3))))
            (V (natural-random (truncate (/ (* MAX_ITEM_PARAM TEST_SIZE) 3))))
            (C (natural-random (truncate (/ (* MAX_ITEM_PARAM TEST_SIZE) 6)))))
        
        (list (generate-items N) (generate-items N) (generate-items N) W V C)))

    ; Generate a false test
    (define (generate-false-test)
      (list (list (generate-items TEST_SIZE) (generate-items TEST_SIZE) (generate-items TEST_SIZE)
                  (* TEST_SIZE MAX_ITEM_PARAM) (* TEST_SIZE MAX_ITEM_PARAM) (* TEST_SIZE MAX_ITEM_PARAM))
            '((#f))))

    ; Generate a true test, where you can take all items
    (define (generate-takeall-test)
      (let ((test (list (generate-items TEST_SIZE)
                        (generate-items TEST_SIZE)
                        (generate-items TEST_SIZE)
                        (* TEST_SIZE MAX_ITEM_PARAM)
                        (* TEST_SIZE MAX_ITEM_PARAM) 1)))
        (list test
              (list (list #t
                          (foldl + 0 (car test))
                          (foldl + 0 (cadr test))
                          (foldl + 0 (caddr test))
                          (build-list TEST_SIZE (lambda (x) (+ x 1))))))))

    ; Get the best solution bu exhaustive search
    (define (get-anwsers test)
      
      (define (exhaustive-search weights volumes costs W V C)
        
        (define (do-search weights volumes costs cur-item cur-items cur-weight cur-volume cur-cost)
          (cond ((and (null? weights) (<= cur-weight W) (<= cur-volume V) (>= cur-cost C))
                 (list (list #t cur-weight cur-volume cur-cost cur-items)))
                ((or (null? weights) (> cur-weight W) (> cur-volume V))
                 (list (list #f 0 0 0 '())))
                (else (let ((branch1
                             (do-search (cdr weights) (cdr volumes) (cdr costs)
                                        (+ cur-item 1) (append cur-items (list cur-item))
                                        (+ cur-weight (car weights)) (+ cur-volume (car volumes)) (+ cur-cost (car costs))))
                            (branch2
                             (do-search (cdr weights) (cdr volumes) (cdr costs)
                                        (+ cur-item 1) cur-items
                                        cur-weight cur-volume cur-cost)))
                        (cond ((> (cadddr (car branch1)) (cadddr (car branch2)))
                               branch1)
                              ((< (cadddr (car branch1)) (cadddr (car branch2)))
                               branch2)
                              (else
                               (append branch1 branch2)))))))
        
        (do-search weights volumes costs 1 '() 0 0 0))

      (let ((result (apply exhaustive-search test)))
        (if (and (> (cadr (car result)) 0) (>= (cadddr (car result)) (car (reverse test))))
            result
            '((#f)))))
    
    (if (= num-tests 0)
        (cons (generate-takeall-test) (cons (generate-false-test) tests))
        (let ((test (generate-test)))
          (generate-tests (- num-tests 1) (cons (list test (get-anwsers test)) tests)))))

  (with-output-to-file "tests.txt"
    (lambda () (print (generate-tests num-tests '())))
    #:mode 'text
    #:exists 'replace))