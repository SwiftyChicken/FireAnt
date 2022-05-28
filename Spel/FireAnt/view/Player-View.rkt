;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Player View ADT is verantwoordelijk voor:
;; [x] Maken, initialiseren en onthouden van de tile
;; [x] Onthouden van tile direction en off het verwijderd is van de laag
;; [x] Onthouden van eigenaar object en de laag waar hij in zit
;; [x] Het reseten en verwijderen van de tile
;; [x] Updaten van de tiles positie en direction
;; [x] Maken van positie verandering "animatie/transitie"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-player-view owner layer)
  (let* ((bitmap (string-append bitmap-dir "ant.png"))
         (mask (string-append mask-dir "ant.png")))

;;;;;;;;;;;;;;;;;;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (get-bitmap)
      bitmap)

    (define (get-mask)
      mask)

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'get-bitmap) (apply get-bitmap args))
            ((eq? cmd 'get-mask) (apply get-mask args))
            (else (error "Unknown command" cmd))))

    dispatch))
