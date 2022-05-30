(load "model/Item.rkt")

(define (new-health position)
  (let ((type 'health)
        (item (new-item position)))

;;;;;;;;;;;;;;;;;;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (get-type)
      type)

;;;;;;;;;;;;;;;;;;; DESTRUCTIVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (take! player)
      (if (not (item 'is-taken?))
        (begin (player 'add-life!)
               (item 'take!))))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'get-type) (apply get-type args))
            ((eq? cmd 'take!) (apply take! args))
            (else (apply item (cons cmd args)))))
    dispatch))
