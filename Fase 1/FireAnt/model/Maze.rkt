;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maze ADT is verantwoordelijk voor:
;; [x] Heeft een type die opgevraagd kan worden
;; [x] Onthouden van de positie van de muren
;; [x] Kan zeggen of een positie een muur is
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-maze)
  (let* ((type 'maze)
        (height GRID-HEIGHT)
        (width GRID-WIDTH)
        (maze (new-matrix height width #f)))
;;;;;;;;;;;;;;;;;;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (get-type)
      type)

    (define (get-height)
      height)

    (define (get-width)
      width)

;;;;;;;;;;;;;;;;;;; SETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (set-wall! row column bool)
      (set-cell! maze row column bool))

;;;;;;;;;;;;;;;;;;; PREDICATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (is-wall? row column)
      (if (and (<= 0 row (- height 1))
               (<= 0 column (- width 1)))
        (get-cell maze row column)
        #t)) ; Invisible wall to not get out of bound

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'get-type) (apply get-type args))
            ((eq? cmd 'set-wall!) (apply set-wall! args))
            ((eq? cmd 'is-wall?) (apply is-wall? args))
            ((eq? cmd 'get-height) (apply get-height args))
            ((eq? cmd 'get-width) (apply get-width args))
            (else (error "Unknown command" cmd))))

    dispatch))
