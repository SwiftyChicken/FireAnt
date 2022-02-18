;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maze ADT is verantwoordelijk voor:
;; [ ] Positie van de muren onthouden
;; [ ] Beweegbare plaatsen in maze aangeven
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-maze)
  (let* ((type 'maze)
        (height 25)
        (width 40)
        (maze (let iter ((v (make-vector height))
                         (i 0))
                (if (= i (vector-length v))
                  v
                  (begin (vector-set! v i (make-vector width #f))
                         (iter v (+ i 1)))))))

    (define (add-wall x y)
      (vector-set! (vector-ref maze y) x #t))

    (define (del-wall x y)
      (vector-set! (vector-ref maze y) x #f))

    (define (is-wall? x y)
      (vector-ref (vector-ref maze y) x))

    (define (dispatch cmd . args)
      (cond ((eq? cmd 'get-type) (apply type args))
            ((eq? cmd 'del-wall!) (apply del-wall args))
            ((eq? cmd 'add-wall!) (apply add-wall args))
            ((eq? cmd 'get-maze) (apply maze args))
            ((eq? cmd 'is-wall?) (apply is-wall? args))
            ((eq? cmd 'get-height) (apply height args))
            ((eq? cmd 'get-width) (apply width args))
            (else (error "Unknown command" cmd))))

    dispatch))
