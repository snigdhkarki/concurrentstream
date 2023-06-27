(define (list-ref lst index)
  (if (null? lst)
      (error "Index out of bounds")
      (if (= index 0)
          (car lst)
          (list-ref (cdr lst) (- index 1)))))

(define (multiply-matrix m1 m2)
  (define (multiply-row row)
    (map (lambda (col)
           (apply + (map * row col)))
         (apply map list m2)))

        (car (map multiply-row (list m1))))

(define (subtract-matrices m1 m2)
  (if (and (= (length m1) 5) (= (length m2) 5))
      (list (- (list-ref m1 0) (list-ref m2 0))
            (- (list-ref m1 1) (list-ref m2 1))
            (- (list-ref m1 2) (list-ref m2 2))
            (- (list-ref m1 3) (list-ref m2 3))
            (- (list-ref m1 4) (list-ref m2 4)))
      (error "Invalid matrix dimensions")))

(define (add-matrices m1 m2)
  (if (and (matrix? m1) (matrix? m2) (= (matrix-rows m1) (matrix-rows m2)) (= (matrix-columns m1) (matrix-columns m2)))
      (let loop-rows ((i 0) (result '()))
        (if (= i (matrix-rows m1))
            (reverse result)
            (let loop-cols ((j 0) (sum '()))
              (if (= j (matrix-columns m1))
                  (loop-rows (+ i 1) (cons (reverse sum) result))
                  (loop-cols (+ j 1) (cons (+ (list-ref (matrix-row m1 i) j) (list-ref (matrix-row m2 i) j)) sum)))))) ; Add corresponding elements
      (error "Matrices must have the same dimensions")))
(define (add-list-of-matrices matrices)
  (let ((result (car matrices)))
    (for-each (lambda (matrix)
                (set! result (add-matrices result matrix)))
              (cdr matrices))
    result))      

(define (matrix? m)
  (and (list? m) (not (null? m)) (list? (car m))))

(define (matrix-rows m)
  (length m))

(define (matrix-columns m)
  (length (car m)))

(define (matrix-row m row-index)
  (list-ref m row-index))
  
(define (sum-modulus m)
  (if (= (length m) 5)
      (let loop ((i 0) (sum 0))
        (if (= i 5)
            sum
            (loop (+ i 1) (+ sum (abs (list-ref m i))))))
      (error "Invalid matrix dimensions")))

(define (divide-by-sum lst)
  (if (not (null? lst))
      (let ((sum (apply + lst)))
        (map (lambda (x) (- 1 (/ x sum))) lst))
      '()))
(define (divide-by-sum-only lst)
  (if (not (null? lst))
      (let ((sum (apply + lst)))
        (map (lambda (x) (/ x sum)) lst))
      '()))
(define (divide-matrix-by-number matrix number)
  (map (lambda (row) (map (lambda (element) (exact->inexact (/ element number))) row))
       matrix))
(define (find-lowest-number numbers)
  (if (null? numbers)
      (error "Empty list")
      (let ((lowest (car numbers)))
        (do ((nums (cdr numbers) (cdr nums)))
            ((null? nums) lowest)
          (let ((current (car nums)))
            (if (< current lowest)
                (set! lowest current) (set! lowest lowest)))))))



 
(define random-state (make-vector 1 0))

(define (random-between-minus-one-and-one)
  (let ((current-state (vector-ref random-state 0)))
    (let ((next-state (modulo (+ (* current-state 1103515245) 12345) (expt 2 32))))
      (vector-set! random-state 0 next-state)
      (exact->inexact (- (/ next-state (expt 2 31)) 1)))))

(define (generate-random-matrix)
  (let ((matrix '()))
    (do ((i 0 (+ i 1)))
        ((= i 5) (reverse matrix))
      (let ((row '()))
        (do ((j 0 (+ j 1)))
            ((= j 5) (set! matrix (cons (reverse row) matrix)))
          (set! row (cons (random-between-minus-one-and-one) row)))))))

(define (generate-random-matrix-list count)
  (let ((matrix-list '()))
    (do ((i 0 (+ i 1)))
        ((= i count) (reverse matrix-list))
      (set! matrix-list (cons (generate-random-matrix) matrix-list)))))

(define (element-wise-multiply coefficient matrix)
  (map (lambda (x) (* coefficient x)) matrix))

(define (get-matrix-for-lowest-number numbers matrices)
  (if (and (not (null? numbers))
           (not (null? matrices)))
      (let* ((min-number (apply min numbers))
             (min-index (position min-number numbers)))
        (if min-index
            (list-ref matrices (- min-index 1))
            (error "No matrix found for the lowest number")))
      (error "Empty lists provided")))

(define (position value lst)
  (let loop ((lst lst) (index 1))
    (cond ((null? lst) #f)
          ((equal? value (car lst)) index)
          (else (loop (cdr lst) (+ index 1))))))

(define input '(1 2 3 4 5))   
(define output '(4 2 5 3 5))
(define initial '((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0)))
(define (multiplywithinput m2) (multiply-matrix input m2))
(define (subtractwithoutput m1) (subtract-matrices m1 output))
(define (parallel-execute . procs)
  (map thread-wait
       (map (lambda (proc) (thread proc))
            procs)))


(define (parallel-find-loss random-matrix-list)    
        (define ls1 0)
        (define ls2 0)
        (define ls3 0)
        (define ls4 0)
        (parallel-execute (lambda () (let ((loss1 (sum-modulus (subtractwithoutput (multiplywithinput (add-matrices initial  (list-ref random-matrix-list 0))))))) (set! ls1 loss1)))
                          (lambda () (let ((loss2 (sum-modulus (subtractwithoutput (multiplywithinput (add-matrices initial  (list-ref random-matrix-list 1))))))) (set! ls2 loss2)))
                          (lambda () (let ((loss3 (sum-modulus (subtractwithoutput (multiplywithinput (add-matrices initial  (list-ref random-matrix-list 2))))))) (set! ls3 loss3)))
                          (lambda () (let ((loss4 (sum-modulus (subtractwithoutput (multiplywithinput (add-matrices initial  (list-ref random-matrix-list 3))))))) (set! ls4 loss4))))
        (list ls1 ls2 ls3 ls4))


(define (looper initial n)
        (define (addwithinitial m1) (add-matrices initial m1))
        (define random-matrix-list (generate-random-matrix-list 4))
        (define loss (parallel-find-loss random-matrix-list))
        (define newmatrix (add-matrices initial (divide-matrix-by-number (get-matrix-for-lowest-number loss random-matrix-list) (find-lowest-number loss))))
        (cond ((= n 0)  initial)
                (else (looper newmatrix (- n 1)))))
                
(display (multiplywithinput (looper initial 100)))









