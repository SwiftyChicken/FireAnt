;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maze ADT is verantwoordelijk voor:
;; [ ] Positie van de muren onthouden
;; [ ] Beweegbare plaatsen in maze aangeven
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-maze)
  (let* ((type 'maze)
        (height 20)
        (width 35)
        (maze (new-matrix height width #f)))
;;;;;;;;;;;;;;;;;;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (get-type)
      type)

    (define (get-maze)
      maze)

    (define (get-height)
      height)

    (define (get-width)
      width)

;;;;;;;;;;;;;;;;;;; SETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (add-wall row column)
      (set-cell! maze row column #t))

    (define (del-wall row column)
      (set-cell! maze row column #f))

;;;;;;;;;;;;;;;;;;; OTHER FUNC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (is-wall? row y)
      (get-cell maze row y))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'get-type) (apply get-type args))
            ((eq? cmd 'del-wall!) (apply del-wall args))
            ((eq? cmd 'add-wall!) (apply add-wall args))
            ((eq? cmd 'get-maze) (apply get-maze args))
            ((eq? cmd 'is-wall?) (apply is-wall? args))
            ((eq? cmd 'get-height) (apply get-height args))
            ((eq? cmd 'get-width) (apply get-width args))
            (else (error "Unknown command" cmd))))

    dispatch))
