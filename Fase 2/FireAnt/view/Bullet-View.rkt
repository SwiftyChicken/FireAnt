(load "view/Movable-View.rkt")

(define (new-bullet-view owner layer)
  (let* ((pic-name "ammo.png")
         (movable (new-movable-view owner layer pic-name pic-name)))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (apply movable (cons cmd args)))

    dispatch))
