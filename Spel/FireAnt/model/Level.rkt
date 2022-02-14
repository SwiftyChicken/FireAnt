(define (new-level player file)
  (define objects '(("#" . #t)
                    (" " . #f)))

  (define (initialize file)
    (let fill-level ((maze (make-vector 25 (make-vector 40 #f))))
      (display maze)
      (newline)))

  (let ((maze (initialize file))
        (ant player))

    (define (start)
      (let ((levels (directory-list "level")))
        (display levels)))

    (define (dispatch-game cmd)
      (cond ((eq? cmd 'start) start)))

    dispatch-game))
