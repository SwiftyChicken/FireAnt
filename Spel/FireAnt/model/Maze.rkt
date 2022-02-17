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

    (define (dispatch cmd)
      (cond ((eq? cmd 'get-type) type)
            ((eq? cmd 'del-wall!) del-wall)
            ((eq? cmd 'add-wall!) add-wall)
            ((eq? cmd 'get-maze) maze)
            ((eq? cmd 'is-wall?) is-wall?)
            ((eq? cmd 'get-height) height)
            ((eq? cmd 'get-width) width)))

    dispatch))
