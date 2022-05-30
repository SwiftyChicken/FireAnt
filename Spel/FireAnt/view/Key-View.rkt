;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Egg View ADT is verantwoordelijk voor:
;; [x] Maken, initialiseren en onthouden van de tile
;; [x] Verwijder tile als wanneer nodig
;; [x] Checken of een object de eigenaar is
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "view/Item-View.rkt")

(define (new-egg-view owner layer)
  (let* ((bitmap (string-append bitmap-dir (case (owner 'get-color)
                                             ((bronze) "egg-bronze.png")
                                             ((silver) "egg-silver.png")
                                             ((gold) "egg-gold.png"))))
         (mask (string-append mask-dir "egg.png"))
         (item-view (new-item-view owner layer bitmap mask)))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (apply item-view (cons cmd args)))

    dispatch))
