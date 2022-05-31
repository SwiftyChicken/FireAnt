(load "view/Character-View.rkt")

(define (new-bullet-view owner layer)
  (let* ((pic-name "ammo.png")
         (character (new-character-view owner layer pic-name pic-name)))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (apply character (cons cmd args)))

    dispatch))
