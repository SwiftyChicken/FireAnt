(load "model/Item.rkt")

(define (new-ammo position)
  (let ((type 'ammo)
        (item (new-item position)))

;;;;;;;;;;;;;;;;;;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (get-type)
      type)

;;;;;;;;;;;;;;;;;;; DESTRUCTIVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (take! player)
        (item 'take! player type 1))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'get-type) (apply get-type args))
            ((eq? cmd 'take!) (apply take! args))
            (else (apply item (cons cmd args)))))

    dispatch))