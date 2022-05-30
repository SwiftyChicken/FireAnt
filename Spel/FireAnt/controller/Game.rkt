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
    (let* ((player (new-player))
           (levels (init-levels))
           (current-level (new-level player 
                                     (car levels)))
           (scoreboard (new-scoreboard player (string-append score-dir "high-score.txt")))
           (view (new-view player current-level scoreboard)))

;;;;;;;;;;;;;;;;;;; DESTRUCTIVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (define (next-level!) ;; Update variables and return next level
        (if (not (null? (cdr levels)))
          (begin (scoreboard 'next-level!)
                 (set! levels (cdr levels))
                 (set! current-level (new-level player 
                                                (car levels)))
                 (view 'set-level! current-level))
          (begin (scoreboard 'save-high-score))))

      (define (reset-levels!)
        (scoreboard 'save-high-score)
        (set! levels (init-levels))
        (scoreboard 'reset-level!))
;;;;;;;;;;;;;;;;;;; KEY HANDLER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (define (key-handler state key)
        (cond ((eq? state 'pressed)
               (case key
                 ((up down left right) (if (current-level 'is-legal-move? player key)
                                         ((player 'get-position) 'move! key)
                                         (current-level 'try-opening! player key)))))))

;;;;;;;;;;;;;;;;;;; GAME LOOP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (define (game-loop ms)
        (if (current-level 'is-finished? player)
          (next-level!)
          (begin (current-level 'update! ms) ; Update all the models
                 (view 'update! ms) ; Update all the views
                 (current-level 'clear-updates!) ; Clean up updates list after updating
                 (if (player 'is-dead?)
                   (current-level 'respawn)))))

;;;;;;;;;;;;;;;;;;; START LOOP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ((view 'key-handler) key-handler)
      ((view 'game-loop) game-loop)))
       

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (define (dispatch cmd . args)
        (cond ((eq? cmd 'start) (apply start args))
              (else (error "Unknown command" cmd))))

      dispatch)
