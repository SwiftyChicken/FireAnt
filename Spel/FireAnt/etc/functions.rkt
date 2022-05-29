;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enkele hulp functies die globaal worden gebruikt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (list->circular lst)
	(let link ((current lst))
		(if (null? (cdr current))
			(begin (set-cdr! current lst)
						 (cdr current))
			(link (cdr current)))))

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
(define (get-random-direction)
	(vector-ref DIRECTIONS (random (vector-length DIRECTIONS))))

(define (coin-flip succes-rate)
	(< (random 100) succes-rate))

;;;;;;;;;;;;;;;;;;; DIRECTION FUNC ;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-opposite direction)
	(case direction
		((up) 'down)
		((down) 'up)
		((left) 'right)
		((right) 'left)))

