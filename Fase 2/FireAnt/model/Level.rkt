(load "model/Maze.rkt")
(load "model/Egg.rkt")
(load "model/Key.rkt")
(load "model/Board.rkt")
(load "model/Health.rkt")
(load "model/Poison.rkt")
(load "model/Ammo.rkt")
(load "model/Bullet.rkt")
(load "model/Surf.rkt")
(load "model/Door.rkt")
(load "model/Scorpion.rkt")
(load "model/Position.rkt")

(define (new-level player map-file)

  (let ((spawn (new-position 0 0))
        (surf #f)
        (kill-all #f)
        (maze (new-maze))
        (scorpions '())
        (items '())
        (doors '())
        (bullets '())
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

    (define (get-items)
      items)

    (define (get-doors)
      doors)

    (define (get-scorpions)
      scorpions)

    (define (get-updates)
      updates)

;;;;;;;;;;;;;;;;;;; PREDICATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (is-finished? player) ;; player has reached the exit
      (set! finished (eq? ((player 'get-position) 'get-y)
                          (- (maze 'get-height) 1)))
          finished)

    (define (is-accessible? position)
      (let* ((x (position 'get-x))
             (y (position 'get-y)))
        (maze 'is-accessible? y x)))

    (define (is-legal-move? object direction)
      (let* ((pos (object 'get-position))
             (peek-pos (pos 'peek direction)))
        (is-accessible? peek-pos)))

    (define (is-intersection? object direction)
      (if (member direction (list 'up 'down))
        (or (is-legal-move? object 'left)
            (is-legal-move? object 'right))
        (or (is-legal-move? object 'up)
            (is-legal-move? object 'down))))

;;;;;;;;;;;;;;;;;;; DESTRUCTIVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (update! ms)
      (add-update! player)

      ; Remove all the unused elements out of the list
      (set! scorpions (filter (lambda (scorpion) (scorpion 'is-alive?)) scorpions))
      (set! items (filter (lambda (item) (not (item 'is-taken?))) items))
      (set! bullets (filter (lambda (bullet) (not (bullet 'has-collided?))) bullets))

      ; Remove Surf(plank) if it isn't used
      (if (and surf (surf 'has-arrived?))
        (set! surf #f))

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

      ; Move all the bullets
      (for-each (lambda (bullet)
                  (if (is-accessible? (bullet 'get-position))
                    (bullet 'update!)
                    (bullet 'collide!))
                  (add-update! bullet))
                bullets)

      ; Move surf plank if it exists
      (if surf
        (let* ((direction (surf 'get-direction))
               (pos (surf 'get-position))
               (next-pos (pos 'peek direction))
               (x (pos 'get-x))
               (y (pos 'get-y))
               (x-next (next-pos 'get-x))
               (y-next (next-pos 'get-y))
               (current (maze 'get-unit y x))
               (next (maze 'get-unit y-next x-next)))
          (if (not (or (eq? next 'water)
                       (eq? next 'empty)))
            (surf 'turn-back!))
          (case current 
            ((water) (surf 'update!)) 
            ((empty) (surf 'arrive!)))
          (add-update! surf)))

      ; Check for collisions
      (on-collision player
                    (lambda (item)
                      (item 'take! player)
                      (add-update! item)) items)

      (on-collision player
                    (lambda (scorpion)
                      (player 'die!)) scorpions)

      (for-each (lambda (bullet)
                  (on-collision bullet
                                (lambda (scorpion)
                                  (scorpion 'die!)
                                  (bullet 'collide!)) scorpions))
                bullets)
      (if kill-all
        (for-each (lambda (scorpion) 
                  (scorpion 'die!)) 
                scorpions)))

    (define (kill-all!)
      (set! kill-all #t))

    (define (try-shooting! player)
        (let* ((position ((player 'get-position) 'copy))
               (direction ((player 'get-position) 'get-orientation)))
          (if (is-legal-move? player direction)
            (begin (player 'use! 'ammo)
                   (set! bullets (cons (new-bullet position direction) bullets))))))

    (define (try-surfing! player direction)
      (if (not (zero? (player 'get-boards)))
        (let* ((water-pos ((player 'get-position) 'peek direction))
               (x (water-pos 'get-x))
               (y (water-pos 'get-y)))
          (if (eq? (maze 'get-unit y x) 'water)
            (let* ((position (player 'get-position)))
              (if (not (position 'is-moving?))
                (begin (position 'move! direction) ; Move the player on the water
                       (set! surf (new-surf position direction))
                       (player 'use! 'board))))))))

    (define (try-opening! player direction)
      (if (not (zero? (player 'get-keys)))
        (let* ((door-pos ((player 'get-position) 'peek direction))
               (door (find-object door-pos doors 
                               (lambda (door to-find)
                                 ((door 'get-position) 'is-colliding? to-find)))))

          (if door ; If the peeked position is a door
            (begin (player 'use! 'key)
                   (door 'open!)
                   (maze 'clear-path! (door-pos 'get-y) (door-pos 'get-x))
                   ((player 'get-position) 'move! direction) ; Player can walk through the door
                   (add-update! door))))))

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
    (define (on-collision target to-do objects) ;; Check collisions for a list of objects and apply a function
      (if (pair? objects)
          (let* ((object (car objects))
                 (obj-pos (object 'get-position))
                 (target-pos (target 'get-position))
                 (rest (cdr objects)))
            (if (obj-pos 'is-colliding? target-pos)
              (to-do object))
            (on-collision target to-do rest))))

    ;; Interpret needs a text of at least length 2
    (define (interpret! text x y)
      (let ((code (substring text 0 CODE-LENGTH)) ;; First 2 characters represent the object type
            (arg (list-tail (string->list text) CODE-LENGTH))) ;; The other characters are used as arguments for object creation
        (cond ((string=? code "  ") (maze 'clear-path! y x))
              ((string=? code "[]") (maze 'add! y x 'wall))
              ((string=? code "{}") (maze 'add! y x 'door)
                                    (set! doors (cons (new-door (new-position x y))
                                                      doors)))
              ((string=? code "##") (maze 'add! y x 'water))
              ((string=? code "SP") (set! spawn (new-position x y))
                                     (respawn))
              ((string=? code "EB") (set! items (cons (new-egg (new-position x y) 'bronze)
                                                     items)))
              ((string=? code "ES") (set! items (cons (new-egg (new-position x y) 'silver)
                                                     items)))
              ((string=? code "EG") (set! items (cons (new-egg (new-position x y) 'gold)
                                                     items)))
              ((string=? code "KY") (set! items (cons (new-key (new-position x y))
                                                     items)))
              ((string=? code "BD") (set! items (cons (new-board (new-position x y))
                                                     items)))
              ((string=? code "<3") (set! items (cons (new-health (new-position x y))
                                                     items)))
              ((string=? code "XX") (set! items (cons (new-poison (new-position x y))
                                                     items)))
              ((string=? code "AM") (set! items (cons (new-ammo (new-position x y))
                                                     items)))
              ((string=? code "SY") (set! scorpions (cons (new-scorpion 'yellow (new-position x y) arg)
                                                          scorpions)))
              ((string=? code "SG") (set! scorpions (cons (new-scorpion 'green (new-position x y) (get-random-direction))
                                                          scorpions)))
              (else (error "Unkown code in level file" code)))))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'get-maze) (apply get-maze args))
            ((eq? cmd 'get-items) (apply get-items args))
            ((eq? cmd 'get-doors) (apply get-doors args))
            ((eq? cmd 'get-scorpions) (apply get-scorpions args))
            ((eq? cmd 'get-updates) (apply get-updates args))
            ((eq? cmd 'is-finished?) (apply is-finished? args))
            ((eq? cmd 'is-legal-move?) (apply is-legal-move? args))
            ((eq? cmd 'update!) (apply update! args))
            ((eq? cmd 'kill-all!) (apply kill-all! args))
            ((eq? cmd 'clear-updates!) (apply clear-updates! args))
            ((eq? cmd 'try-shooting!) (apply try-shooting! args))
            ((eq? cmd 'try-opening!) (apply try-opening! args))
            ((eq? cmd 'try-surfing!) (apply try-surfing! args))
            ((eq? cmd 'respawn) (apply respawn args))
            (else error "Unknown command" cmd)))

    (init map-file)
    dispatch))
