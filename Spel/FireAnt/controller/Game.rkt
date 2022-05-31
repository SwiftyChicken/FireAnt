(#%require compatibility/mlist (only racket
                                     directory-list
                                     path->string))

(load "model/Player.rkt")
(load "model/Level.rkt")
(load "model/Scoreboard.rkt")
(load "view/View.rkt")

(define (new-game)
  (define (init-levels)
    ;; List all levels in level-dir
    (map (lambda (path) (string-append level-dir (path->string path)))
         (list->mlist (directory-list level-dir))))

  (define (start)
    (let* ((game-running #t) ; Check if game is over or not
           (player (new-player))
           (levels (init-levels))
           (current-level (new-level player 
                                     (car levels)))
           (scoreboard (new-scoreboard player HIGH-SCORE-FILE))
           (view (new-view player current-level scoreboard)))

;;;;;;;;;;;;;;;;;;; DESTRUCTIVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (define (next-level!) ;; Update variables and return next level
        (if (pair? (cdr levels))
          (begin (scoreboard 'next-level!)
                 (set! levels (cdr levels))
                 (set! current-level (new-level player 
                                                (car levels)))
                 (view 'set-level! current-level))
          (begin (scoreboard 'save-high-score)
                 (set! game-running #f))))

      (define (reset-game!)
        (scoreboard 'save-high-score)
        (player 'reset!)
        (set! levels (init-levels))
        (set! current-level (new-level player 
                                       (car levels)))
        (view 'set-level! current-level)
        (scoreboard 'reset-level!)
        (set! game-running #t))

;;;;;;;;;;;;;;;;;;; KEY HANDLER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (define (key-handler state key)
        (case state 
          ((pressed)
           (case key
             ((up down left right) (if (current-level 'is-legal-move? player key)
                                     ((player 'get-position) 'move! key)
                                     (current-level 'try-opening! player key)))
             ;; CHEAT CODES
             ((#\r) (player 'reset!)
                    (set! current-level (new-level player 
                                                   (car levels)))
                    (view 'set-level! current-level))
             ((#\n) (next-level!))))
              ((released)
               (case key
                 ((#\space) (if (> (player 'get-ammo) 0)
                              (current-level 'try-shooting! player)))))))

;;;;;;;;;;;;;;;;;;; GAME LOOP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (define (game-loop ms)
        (if game-running
          (if (current-level 'is-finished? player)
            (next-level!)
            (begin (current-level 'update! ms) ; Update all the models
                   (view 'update! ms) ; Update all the views
                   (current-level 'clear-updates!) ; Clean up updates list after updating
                   (if (player 'is-dead?)
                     (if (> (player 'get-lives) 0)
                       (current-level 'respawn)
                       (set! game-running #f))))) ; GAME OVER
          (reset-game!)))

;;;;;;;;;;;;;;;;;;; START LOOP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ((view 'key-handler) key-handler)
      ((view 'game-loop) game-loop)))
       
;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (define (dispatch cmd . args)
        (cond ((eq? cmd 'start) (apply start args))
              (else (error "Unknown command" cmd))))

      dispatch)
