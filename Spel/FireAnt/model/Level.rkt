(define (new-level player file)
  (define obj-dict '(("#" . #t)
                    (" " . #f)))

  (let ((ant player)
        (scorpions '())
        (eggs '())
        (maze (maze (make-vector 25 (make-vector 40 #f))))
        (finished #f))

    (define (init)
      ())

    (define (dispatch cmd)
      (cond ((eq? cmd 'init) init)))

    dispatch))
