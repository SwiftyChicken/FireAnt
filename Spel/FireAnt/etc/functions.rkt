;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enkele hulp functies die globaal worden gebruikt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (list->circular lst) ; transform list in circular list
	(let link ((current lst))
		(if (null? (cdr current))
			(begin (set-cdr! current lst)
						 (cdr current))
			(link (cdr current)))))

    (define (find-object to-find objects comparator) ; find object in list of objects
      (if (pair? objects)
        (let ((object (car objects)))
          (if (comparator object to-find)
            view
            (find-object to-find (cdr views) comparator)))
        #f))

;;;;;;;;;;;;;;;;;;; MATRIX FUNC ;;;;;;;;;;;;;;;;;;;;;;;;;
(define (new-matrix height width blank)
	(let iter ((vect (make-vector height))
             (i 0))
		(if (= i (vector-length vect))
			vect
			(begin (vector-set! vect i (make-vector width blank))
						 (iter vect (+ i 1))))))

(define (get-cell matrix row column)
	(vector-ref (vector-ref matrix row) column))

(define (set-cell! matrix row column new-value)
	(vector-set! (vector-ref matrix row) column new-value))

;;;;;;;;;;;;;;;;;;; RNG FUNC ;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-random-direction) ; give back a random direction
	(vector-ref DIRECTIONS (random (vector-length DIRECTIONS))))

(define (coin-flip succes-rate) ; gives back #t for succes-rate% of the time
	(< (random 100) succes-rate))

;;;;;;;;;;;;;;;;;;; DIRECTION FUNC ;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-opposite direction)
	(case direction
		((up) 'down)
		((down) 'up)
		((left) 'right)
		((right) 'left)))

