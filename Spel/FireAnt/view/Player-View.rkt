(load "view/Movable-View.rkt")

(define (new-player-view owner layer)
  (let* ((pic-name "ant.png")
         (movable (new-movable-view owner layer pic-name pic-name)))

;;;;;;;;;;;;;;;;;;; DESTRUCTIVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (reset!)
      (set! movable (new-movable-view owner layer pic-name pic-name)))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'reset!) (apply reset! args))
            (else (apply movable (cons cmd args)))))
    dispatch))
