(load "model/Item.rkt")

(define (new-key position)
  (let ((type 'key)
        (item (new-item position)))

;;;;;;;;;;;;;;;;;;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (get-type)
      type)

;;;;;;;;;;;;;;;;;;; DESTRUCTIVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (take! player)
      (item 'take!) 
      (player 'take-key!))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'get-type) (apply get-type args))
            ((eq? cmd 'take!) (apply take! args))
            (else (apply item (cons cmd args)))))

    dispatch))
