;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Level ADT is verantwoordelijk voor:
;; [ ] Initialiseert nodige model objecten
;; [ ] 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "model/Maze.rkt")
(load "view/View_Maze.rkt")

(define (new-level player map-file)

  (let ((ant player)
        (scorpions '())
        (eggs '())
        (maze (new-maze))
        (finished #f))
    (define (init file)
      (call-with-input-file file
                            (lambda (input-port)
                              (let iter ((cell (string (read-char input-port)))
                                         (next (peek-char input-port))
                                         (x 0)
                                         (y 0))
                                (if (eof-object? next)
                                  (display cell)
                                  (case next 
                                    ((#\,) (init-obj cell x y)
                                           (read-char input-port)
                                           (iter (string (read-char input-port))
                                                 (peek-char input-port)
                                                 (+ x 1) y))
                                    ((#\newline) (init-obj cell x y)
                                                 (read-char input-port)
                                                 (if (not (eof-object? (peek-char input-port)))
                                                   (iter (string (read-char input-port))
                                                       (peek-char input-port)
                                                       0 (+ y 1))))
                                    (else (iter (string-append cell (string (read-char input-port)))
                                                (peek-char input-port) 
                                                x y))))))))

    (define (init-obj obj x y)
      (cond ((string=? obj "#") ((maze 'add-wall!) x y))
            ((string=? obj " ") ((maze 'del-wall!) x y))
            ((string=? obj "entry") (display "player position")
                                    (display (cons x y)))
            (else (display obj) ;; Debugging
                  (display "->")
                  (display (string-length obj))
                  (newline))))

    (define (dispatch cmd)
      (cond ((eq? cmd 'restart) init)
            ((eq? cmd 'get-scorpions) scorpions)
            ((eq? cmd 'get-eggs) eggs)
            ))

    (init map-file)
    dispatch))
