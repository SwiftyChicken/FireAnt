(load "view/Character-View.rkt")

(define (new-player-view owner layer)
  (let* ((pic-name "ant.png")
         (character (new-character-view owner layer pic-name pic-name)))

;;;;;;;;;;;;;;;;;;; DESTRUCTIVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (reset!)
      (set! character (new-character-view owner layer pic-name pic-name)))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'reset!) (apply reset! args))
            (else (apply character (cons cmd args)))))
    dispatch))
