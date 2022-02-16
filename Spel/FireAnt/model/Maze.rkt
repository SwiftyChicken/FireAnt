(define (new-maze)
  (let ((type 'maze)
        (maze (let iter ((v (make-vector 25))
                         (i 0))
                (if (= i (vector-length v))
                  v
                  (begin (vector-set! v i (make-vector 40 #f))
                         (iter v (+ i 1)))))))

    (define (add-wall x y)
      (vector-set! (vector-ref maze y) x #t))

    (define (del-wall x y)
      (vector-set! (vector-ref maze y) x #f))

    (define (dispatch cmd)
      (cond ((eq? cmd 'del-wall!) del-wall)
            ((eq? cmd 'add-wall!) add-wall)
            ((eq? cmd 'get-maze) maze)))

    dispatch))
