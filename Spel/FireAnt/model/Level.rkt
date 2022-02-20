;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Level ADT is verantwoordelijk voor:
;; [ ] Initialiseert nodige model objecten
;; [ ] 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "model/Maze.rkt")
(load "model/Egg.rkt")
(load "model/Scorpion.rkt")
(load "model/Position.rkt")

(define (new-level player map-file)

  (let ((spawn (new-position 0 0))
        (scorpions '())
        (eggs '())
        (maze (new-maze))
        (updates '())
        (finished #f))
;;;;;;;;;;;;;;;;;;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (get-scorpions)
      scorpions)

    (define (get-eggs)
      eggs)

    (define (get-maze)
      maze)

    (define (get-updates)
      updates)

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


    (define (init-obj text x y)
      (let ((code (string (string-ref text 0)
                          (string-ref text 1)))
            (arg (list-tail (string->list text) 2)))
        (cond ((string=? code "[]") (maze 'add-wall! y x))
            ((string=? code "  ") (maze 'del-wall! y x))
            ((string=? code "SP") (set! spawn (new-position x y))
                                     (respawn player))
            ((string=? code "EG") (set! eggs (cons (new-egg (new-position x y))
                                                   eggs)))
            ((string=? code "SY") (set! scorpions (cons (new-scorpion (new-position x y) arg)
                                                        scorpions)))
            (else (error "Unkown code in level file" code)))))

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

    (define (update)
      ; Clear old updates
      (set! updates (list player))
      ; Move Scorpions

      ; Check for collisions
      (let iter ((lst eggs))
        (if (not (null? lst))
          (let* ((egg (car lst))
               (egg-pos (egg 'get-position))
               (player-pos (player 'get-position))
               (rest (cdr lst)))
          (if (egg-pos 'is-equal? player-pos)
            (begin (egg 'take!)
                   (set! updates (cons egg updates)))
            (iter (cdr lst)))))))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'update) (apply update args))
            ((eq? cmd 'respawn) (apply respawn args))
            ((eq? cmd 'is-finished) (apply is-finished args))
            ((eq? cmd 'get-scorpions) (apply get-scorpions args))
            ((eq? cmd 'get-eggs) (apply get-eggs args))
            ((eq? cmd 'get-maze) (apply get-maze args))
            ((eq? cmd 'get-updates) (apply get-updates args))
            ((eq? cmd 'move-player) (apply move-player args))
            (else error "Unknown command" cmd)))

    (init map-file)
    dispatch))
