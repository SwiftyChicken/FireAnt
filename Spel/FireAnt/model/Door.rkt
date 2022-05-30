(define (new-door pos)
  (let ((type 'door)
        (position pos)
        (open #f))

;;;;;;;;;;;;;;;;;;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (get-type)
      type)

    (define (get-position)
      position)

;;;;;;;;;;;;;;;;;;; DESTRUCTIVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (open!)
      (set! open #t))

;;;;;;;;;;;;;;;;;;; PREDICATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (is-open?)
      open)

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'get-type) (apply get-type args))
            ((eq? cmd 'get-position) (apply get-position args))
            ((eq? cmd 'is-open?) (apply is-open? args))
            ((eq? cmd 'open!) (apply open! args))
            (else (error "Unkown command" cmd))))

    dispatch))
