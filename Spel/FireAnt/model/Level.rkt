;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Level ADT is verantwoordelijk voor:
;; [ ] Initialiseert nodige model objecten
;; [ ] 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "model/Maze.rkt")
(load "model/Position.rkt")

(define (new-level player map-file)

  (let ((spawn (new-position 0 0))
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
            ((string=? obj "entry") (set! spawn (new-position x y))
                                    ((player 'set-position) (spawn 'get-position)))
            (else (display obj) ;; Debugging
                  (display "->")
                  (display (string-length obj))
                  (newline))))

    (define (respawn player)
      ((player 'set-position) (spawn 'get-position)))


    (define (is-finished player) ;; player has reached the exit
      (set! finished (eq? ((player 'get-positie) 'get-y)
                          (- (maze 'get-height) 1)))
          finished)

    (define (dispatch cmd)
      (cond ((eq? cmd 'respawn) respawn)
            ((eq? cmd 'is-finished) is-finished)
            ((eq? cmd 'get-scorpions) scorpions)
            ((eq? cmd 'get-eggs) eggs)))

    (init map-file)
    dispatch))
