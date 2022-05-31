(load "view/Movable-View.rkt")

(define (new-scorpion-view owner layer)
  (define (get-bitmap-file color)
    (let ((yellow_pic "scorpion_yellow.png")
          (green_pic "scorpion_green.png")
          (purple_pic "scorpion_purple.png"))
      (case color
                                             ((yellow) yellow_pic)
                                             ((green) green_pic)
                                             ((purple) purple_pic)
                                             (else (error "Unkown color" color)))))

;;;;;;;;;;;;;;;;;;; DISPATCH LET ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let* ((color (owner 'get-color))
         (bitmap (get-bitmap-file color))
         (mask "scorpion.png")
         (movable (new-movable-view owner layer bitmap mask)))

;;;;;;;;;;;;;;;;;;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (get-bitmap)
      bitmap)

    (define (get-mask)
      mask)
    
;;;;;;;;;;;;;;;;;;; DESTRUCTIVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (update-color!)
      (if (not (movable 'is-moving?))
        (let ((owner-color (owner 'get-color)))
          (if (not (eq? color owner-color))
            (begin (set! color owner-color)
                   (set! bitmap (get-bitmap-file color))
                   ((layer 'remove-drawable) (movable 'get-tile))
                   (set! movable (new-movable-view owner layer bitmap mask)))))))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'get-bitmap) (apply get-bitmap args))
            ((eq? cmd 'get-mask) (apply get-mask args))
            ((eq? cmd 'update-color!) (apply update-color! args))
            (else (apply movable (cons cmd args)))))

    dispatch))
