(define (new-item pos)
  (let ((position pos)
        (taken #f))
;;;;;;;;;;;;;;;;;;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (get-position)
      position)

;;;;;;;;;;;;;;;;;;; DESTRUCTIVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (take!)
      (set! taken #t))

;;;;;;;;;;;;;;;;;;; PREDICATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (is-taken?)
      taken)

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'get-position) (apply get-position args))
            ((eq? cmd 'is-taken?) (apply is-taken? args))
            ((eq? cmd 'take!) (apply take! args))
            (else (error "Unkown command" cmd))))
    dispatch))
