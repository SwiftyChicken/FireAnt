;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scorpion ADT is verantwoordelijk voor:
;; [x] Heeft een type die opgevraagd kan worden
;; [x] Onthoud zijn positie en route (stappen plan)
;; [x] Kan een gegeven path interpreteren naar een bruikbare circulaire lijst
;; [x] Updaten van zijn beweging volgens de route plan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-scorpion-green position direction)
;;;;;;;;;;;;;;;;;;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (get-direction)
      direction)

;;;;;;;;;;;;;;;;;;; DESTRUCTIVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (update!)
      (if (not (position 'is-moving?))
        (position 'move! direction)))

    (define (turn-back!)
      (case direction
        ((up) (set! direction 'down))
        ((down) (set! direction 'up))
        ((right) (set! direction 'left))
        ((left) (set! direction 'right))))

    (define (set-direction! new-direction)
            (set! direction new-direction))
;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'get-direction) (apply get-direction args))
            ((eq? cmd 'turn-back!) (apply turn-back! args))
            ((eq? cmd 'set-direction!) (apply set-direction! args))
            ((eq? cmd 'update!) (apply update! args))
            (else (error "Unkown command" cmd))))
    dispatch)
