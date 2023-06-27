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




(define input '(1 2 3 4 5))   
(define output '(4 2 5 3 5))


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

(define (multiplywithinput m2) (multiply-matrix input m2))

(define (subtract-matrices m1 m2)
  (if (and (= (length m1) 5) (= (length m2) 5))
      (list (- (list-ref m1 0) (list-ref m2 0))
            (- (list-ref m1 1) (list-ref m2 1))
            (- (list-ref m1 2) (list-ref m2 2))
            (- (list-ref m1 3) (list-ref m2 3))
            (- (list-ref m1 4) (list-ref m2 4)))
      (error "Invalid matrix dimensions")))

(define (subtractwithoutput m1) (subtract-matrices m1 output))

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


;;(define result (get-matrix-for-lowest-number numbers matrices))

(define (add-matrices m1 m2)
  (if (and (matrix? m1) (matrix? m2) (= (matrix-rows m1) (matrix-rows m2)) (= (matrix-columns m1) (matrix-columns m2)))
      (let ((result '()))
        (do ((i 0 (+ i 1)))
            ((= i (matrix-rows m1)) (reverse result))
          (let ((row1 (matrix-row m1 i))
                (row2 (matrix-row m2 i))
                (sum '()))
            (do ((j 0 (+ j 1)))
                ((= j (matrix-columns m1)) (set! result (cons (reverse sum) result)))
              (set! sum (cons (+ (list-ref row1 j) (list-ref row2 j)) sum)))))) ; Add corresponding elements
      (error "Matrices must have the same dimensions")))

(define (matrix? m)
  (and (list? m) (not (null? m)) (list? (car m))))

(define (matrix-rows m)
  (length m))

(define (matrix-columns m)
  (length (car m)))

(define (matrix-row m row-index)
  (list-ref m row-index)) ; No need to subtract 1 from row index

(define initial (list (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0)))









(define random-matrix-list (generate-random-matrix-list 4))
(define results (map multiplywithinput random-matrix-list))
(define loss (map sum-modulus (map subtractwithoutput results)))
(define newmatrix (add-matrices (initial (get-matrix-for-lowest-number loss random-matrix-list))))





