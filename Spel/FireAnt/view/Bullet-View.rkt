(load "view/Character-View.rkt")

(define (new-bullet-view owner layer)
  (let* ((pic-name "ammo.png")
         (character (new-character-view owner layer pic-name pic-name)))

;;;;;;;;;;;;;;;;;;; DESTRUCTIVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (remove!)
        (layer 'empty))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond 
            ((eq? cmd 'remove!) (apply remove! args))
            (else (apply character (cons cmd args)))))
    dispatch))
