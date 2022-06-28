(load "view/Item-View.rkt")

(define (new-egg-view owner layer)
  (let* ((bitmap-pic (case (owner 'get-color)
                   ((bronze) "egg-bronze.png")
                   ((silver) "egg-silver.png")
                   ((gold) "egg-gold.png")))
         (mask-pic "egg.png")
         (item-view (new-item-view owner layer bitmap-pic mask-pic)))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (apply item-view (cons cmd args)))

    dispatch))
