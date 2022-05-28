;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scorpion ADT is verantwoordelijk voor:
;; [x] Heeft een type die opgevraagd kan worden
;; [x] Onthoud zijn positie en route (stappen plan)
;; [x] Kan een gegeven path interpreteren naar een bruikbare circulaire lijst
;; [x] Updaten van zijn beweging volgens de route plan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "model/Scorpion-Green.rkt")
(load "model/Scorpion-Yellow.rkt")

(define (new-scorpion color position args)
;;;;;;;;;;;;;;;;;;; DISPATCH LET ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let* ((type 'scorpion)
         (scorpion-type (case color
                          ((yellow) (new-scorpion-yellow position args))
                          ((green) (new-scorpion-green position args))
                          (else (error "Unkown color" color)))))

;;;;;;;;;;;;;;;;;;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (get-type)
      type)

    (define (get-position)
      position)

    (define (get-color)
      color)

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'get-type) (apply get-type args))
            ((eq? cmd 'get-position) (apply get-position args))
            ((eq? cmd 'get-color) (apply get-color args))
            (else (apply scorpion-type (cons cmd args)))))
    dispatch))
