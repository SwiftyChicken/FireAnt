;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schorpioen View ADT is verantwoordelijk voor:
;; [x] Maken, initialiseren en onthouden van de tile
;; [x] Onthouden van tile direction en off het verwijderd is van de laag
;; [x] Onthouden van eigenaar object en de laag waar hij in zit
;; [x] Checken of een object de eigenaar is
;; [x] Updaten van de tiles positie en direction
;; [x] Maken van positie verandering "animatie/transitie"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-scorpion-view owner layer)
  (let* ((bitmap (string-append bitmap-dir (case (owner 'get-color)
                                             ((yellow) "scorpion_y.png")
                                             ((green) "scorpion_g.png"))))
         (mask (string-append mask-dir "scorpion.png")))

;;;;;;;;;;;;;;;;;;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (get-bitmap)
      bitmap)

    (define (get-mask)
      mask)
    
;;;;;;;;;;;;;;;;;;; PREDICATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (is-owner? object)
      (eq? object owner))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'get-bitmap) (apply get-bitmap args))
            ((eq? cmd 'get-mask) (apply get-mask args))
            ((eq? cmd 'is-owner?) (apply is-owner? args))
            (else (error "Unknown command" cmd))))

    dispatch))
