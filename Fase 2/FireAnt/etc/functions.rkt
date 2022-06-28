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
            object
            (find-object to-find (cdr objects) comparator)))
        #f))

		(define (filter test lst)
			(if (null? lst)
				lst
				(let ((element (car lst))
							(rest (cdr lst)))
					(if (test element)
						(cons element (filter test rest))
						(filter test rest)))))

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

(define (coin-flip succes-rate) ; gives back #t for a succes-rate per 10000 of the time
	(< (random 10000) succes-rate))

;;;;;;;;;;;;;;;;;;; DIRECTION FUNC ;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-opposite direction)
	(case direction
		((up) 'down)
		((down) 'up)
		((left) 'right)
		((right) 'left)))

