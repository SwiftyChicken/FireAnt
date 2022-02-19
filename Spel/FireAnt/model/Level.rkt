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
;;;;;;;;;;;;;;;;;;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (get-scorpions)
      scorpions)

    (define (get-eggs)
      eggs)

    (define (get-maze)
      maze)

;;;;;;;;;;;;;;;;;;; SETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;; OTHER FUNC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
      (cond ((string=? obj "[]") (maze 'add-wall! y x))
            ((string=? obj "  ") (maze 'del-wall! y x))
            ((string=? obj "SP") (set! spawn (new-position x y))
                                    (respawn player))
            ((string=? obj "SY") #f)
            (else (display obj) ;; Debugging
                  (display "->")
                  (display (string-length obj))
                  (newline))))

    (define (respawn player)
      (let ((x (spawn 'get-x))
            (y (spawn 'get-y)))
        (player 'set-position (new-position x y))))

    (define (move-player direction)
      (let* ((pos (player 'get-position))
             (peek-pos (pos 'peek direction))
             (new-x (peek-pos 'get-x))
             (new-y (peek-pos 'get-y)))
        (if (not (maze 'is-wall? new-y new-x))
          (pos 'move direction))))

    (define (is-finished player) ;; player has reached the exit
      (set! finished (eq? ((player 'get-position) 'get-y)
                          (- (maze 'get-height) 1)))
          finished)

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'respawn) (apply respawn args))
            ((eq? cmd 'is-finished) (apply is-finished args))
            ((eq? cmd 'get-scorpions) (apply get-scorpions args))
            ((eq? cmd 'get-eggs) (apply get-eggs args))
            ((eq? cmd 'get-maze) (apply get-maze args))
            ((eq? cmd 'move-player) (apply move-player args))
            (else error "Unknown command" cmd)))

    (init map-file)
    dispatch))
