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

(define (list->circular lst)
	(let link ((current lst))
		(if (null? (cdr current))
			(begin (set-cdr! current lst)
						 (cdr current))
			(link (cdr current)))))
