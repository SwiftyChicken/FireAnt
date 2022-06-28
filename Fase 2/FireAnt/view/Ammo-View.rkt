(load "view/Item-View.rkt")

(define (new-ammo-view owner layer)
  (let* ((pic-name "ammo.png")
         (item-view (new-item-view owner layer pic-name pic-name)))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (apply item-view (cons cmd args)))

    dispatch))
