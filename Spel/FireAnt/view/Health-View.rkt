(load "view/Item-View.rkt")

(define (new-health-view owner layer)
  (let* ((pic-name "health.png")
         (item-view (new-item-view owner layer pic-name pic-name)))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (apply item-view (cons cmd args)))

    dispatch))
