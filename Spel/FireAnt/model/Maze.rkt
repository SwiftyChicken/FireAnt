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
        (maze (new-matrix height width 'empty)))
;;;;;;;;;;;;;;;;;;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (get-type)
      type)

    (define (get-height)
      height)

    (define (get-width)
      width)

    (define (get-unit row column)
      (if (and (<= 0 row (- height 1))
               (<= 0 column (- width 1)))
        (get-cell maze row column)
        (error "Index out of bound" (cons row column)))) ; out of bound
;;;;;;;;;;;;;;;;;;; SETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (clear-path! row column)
      (set-cell! maze row column 'empty))

    (define (add-wall! row column)
      (set-cell! maze row column 'wall))

    (define (add-door! row column)
      (set-cell! maze row column 'door))

;;;;;;;;;;;;;;;;;;; PREDICATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (is-accessible? row column)
      (if (and (<= 0 row (- height 1))
               (<= 0 column (- width 1)))
        (eq? 'empty (get-cell maze row column))
        #f)) ; out of bound

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'get-type) (apply get-type args))
            ((eq? cmd 'get-unit) (apply get-unit args))
            ((eq? cmd 'clear-path!) (apply clear-path! args))
            ((eq? cmd 'add-wall!) (apply add-wall! args))
            ((eq? cmd 'add-door!) (apply add-door! args))
            ((eq? cmd 'is-accessible?) (apply is-accessible? args))
            ((eq? cmd 'get-height) (apply get-height args))
            ((eq? cmd 'get-width) (apply get-width args))
            (else (error "Unknown command" cmd))))

    dispatch))

