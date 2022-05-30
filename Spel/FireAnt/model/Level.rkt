;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Level ADT is verantwoordelijk voor:
;; [x] Het lezen van een csv tekst bestand en zijn  elementen te interpreteren
;; [x] Onthouden van de start positie, speler, scorpions, eggs en maze
;; [x] Het verplaatsen van de speler naar de start positie wanneer nodig
;; [x] Updaten van de element in de level o.a. Collision detection, Movement update, Object status, etc.
;; [x] Checken of het level uitgespeeld is
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "model/Maze.rkt")
(load "model/Egg.rkt")
(load "model/Door.rkt")
(load "model/Scorpion.rkt")
(load "model/Position.rkt")

(define (new-level player map-file)

  (let ((spawn (new-position 0 0))
        (scorpions '())
        (eggs '())
        (doors '())
        (maze (new-maze))
        (updates '())
        (finished #f))

;;;;;;;;;;;;;;;;;;; INITIALIZATION ;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Initialize by reading the csv file and parsing every element with their position to the interpret function
    (define (init file) 
      (call-with-input-file file
                            (lambda (input-port)
                              (let iter ((cell (string (read-char input-port)))
                                         (next (peek-char input-port))
                                         (x 0)
                                         (y 0))
                                (if (not (eof-object? next))
                                  (case next 
                                    ((#\,) (interpret! cell x y)
                                           (read-char input-port)
                                           (iter (string (read-char input-port))
                                                 (peek-char input-port)
                                                 (+ x 1) y))
                                    ((#\newline) (interpret! cell x y)
                                                 (read-char input-port)
                                                 (if (not (eof-object? (peek-char input-port)))
                                                   (iter (string (read-char input-port))
                                                       (peek-char input-port)
                                                       0 (+ y 1))))
                                    (else (iter (string-append cell (string (read-char input-port)))
                                                (peek-char input-port) 
                                                x y))))))))

;;;;;;;;;;;;;;;;;;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (get-maze)
      maze)

    (define (get-eggs)
      eggs)

    (define (get-scorpions)
      scorpions)

    (define (get-updates)
      updates)

;;;;;;;;;;;;;;;;;;; PREDICATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (is-finished? player) ;; player has reached the exit
      (set! finished (eq? ((player 'get-position) 'get-y)
                          (- (maze 'get-height) 1)))
          finished)

    (define (is-legal-move? object direction)
      (let* ((pos (object 'get-position))
             (peek-pos (pos 'peek direction))
             (new-x (peek-pos 'get-x))
             (new-y (peek-pos 'get-y)))
        (maze 'is-accessible? new-y new-x)))

    (define (is-intersection? object direction)
      (if (member direction (list 'up 'down))
        (or (is-legal-move? object 'left)
            (is-legal-move? object 'right))
        (or (is-legal-move? object 'up)
            (is-legal-move? object 'down))))

;;;;;;;;;;;;;;;;;;; DESTRUCTIVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (update! ms)
      (add-update! player)
      ; Move Scorpions
      (for-each (lambda (scorpion)
                  (scorpion 'try-boosting! ms)
                  (case (scorpion 'get-race)
                    ((green)
                     ; Change direction in intersection
                     (if (is-intersection? scorpion (scorpion 'get-direction))
                               (scorpion 'set-direction! (get-random-direction)))
                     
                     (if (or (not (is-legal-move? scorpion (scorpion 'get-direction))) ; Turn back if it hits a wall
                             (spawn 'is-colliding?                                     ; Prevent Spawn kill
                                    ((scorpion 'get-position)
                                     'peek (scorpion 'get-direction))))
                       (scorpion 'turn-back!))))

                  (scorpion 'update!)
                  (add-update! scorpion))
                scorpions)

      ; Check for collisions
      (on-collision (lambda (egg)
                      (egg 'take!)
                      (add-update! egg)) eggs)
      (on-collision (lambda (scorpion)
                      (player 'die!)) scorpions))

    (define (try-opening! player direction)
      (if (not (zero? (player 'get-keys)))
        (let* ((door-pos ((player 'get-position) 'peek direction))
               (door (find-obj door-pos doors 
                               (lambda (door to-find)
                                 ((door 'get-position) 'is-colliding? to-find)))))
          (player 'use-key!)
          (door 'open!)
          (maze 'clear-path! (door-pos 'get-y) (door-pos 'get-x))
          (add-update! door))))

    (define (add-update! object)
      (set! updates (cons object updates)))

    (define (clear-updates!)
      (set! updates '()))
;;;;;;;;;;;;;;;;;;; NON-DESTRUCTIVE ;;;;;;;;;;;;;;;;;;;;;;;;
    (define (respawn)
      (let ((x (spawn 'get-x))
            (y (spawn 'get-y)))
        (player 'set-position! (new-position x y))
        (player 'revive!)))

;;;;;;;;;;;;;;;;;;; AUXILIARY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (on-collision to-do objects) ;; Check collisions for a list of objects and apply a function
      (if (pair? objects)
          (let* ((object (car objects))
                 (obj-pos (object 'get-position))
                 (player-pos (player 'get-position))
                 (rest (cdr objects)))
            (if (obj-pos 'is-colliding? player-pos)
              (to-do object))
            (on-collision to-do rest))))

    ;; Interpret needs a text of at least length 2
    (define (interpret! text x y)
      (let ((code (substring text 0 2)) ;; First 2 characters represent the object type
            (arg (list-tail (string->list text) 2))) ;; The other characters are used as arguments for object creation
        (cond ((string=? code "  ") (maze 'clear-path! y x))
              ((string=? code "[]") (maze 'add-wall! y x))
              ((string=? code "{}") (maze 'add-door! y x)
                                    (set! doors (cons (new-door (new-position x y))
                                                      doors)))
              ((string=? code "SP") (set! spawn (new-position x y))
                                     (respawn))
              ((string=? code "EG") (set! eggs (cons (new-egg (new-position x y))
                                                     eggs)))
              ((string=? code "SY") (set! scorpions (cons (new-scorpion 'yellow (new-position x y) arg)
                                                          scorpions)))
              ((string=? code "SG") (set! scorpions (cons (new-scorpion 'green (new-position x y) (get-random-direction))
                                                          scorpions)))
              (else (error "Unkown code in level file" code)))))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'get-maze) (apply get-maze args))
            ((eq? cmd 'get-eggs) (apply get-eggs args))
            ((eq? cmd 'get-scorpions) (apply get-scorpions args))
            ((eq? cmd 'get-updates) (apply get-updates args))
            ((eq? cmd 'is-finished?) (apply is-finished? args))
            ((eq? cmd 'is-legal-move?) (apply is-legal-move? args))
            ((eq? cmd 'update!) (apply update! args))
            ((eq? cmd 'clear-updates!) (apply clear-updates! args))
            ((eq? cmd 'try-opening!) (apply try-opening! args))
            ((eq? cmd 'respawn) (apply respawn args))
            (else error "Unknown command" cmd)))

    (init map-file)
    dispatch))
