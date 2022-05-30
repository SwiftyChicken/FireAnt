(load "model/Item.rkt")

(define (new-food position points)
  (let ((item (new-item position)))

    (define (take! player)
      (if (not (item 'is-taken?))
        (begin (item 'take!)
               (player 'add-points! points))))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'take!) (apply take! args))
            (else (apply item (cons cmd args)))))

    dispatch))
