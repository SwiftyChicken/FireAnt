;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game ADT is verantwoordelijk voor:
;; [x] Starten van juiste level
;; [x] Laden van het volgende level
;; [x] Initialisatie van de Player ADT en View ADT
;; [x] Verantwoordelijk voor Spel lus en Speler invoer
;; [x] Laat de speler de mier bewegen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(#%require compatibility/mlist (only racket
                                     directory-list
                                     path->string))

(load "model/Player.rkt")
(load "model/Level.rkt")
(load "model/Scoreboard.rkt")
(load "view/View.rkt")

(define (new-game)
  (define (start)
    (let* ((player (new-player))
           (levels (map (lambda (path) ;; List all levels in level-dir
                          (string-append level-dir (path->string path)))
                        (list->mlist (directory-list level-dir))))
           (current-level (new-level player 
                                     (car levels)))
           (scoreboard (new-scoreboard player (string-append score-dir "high-score.txt")))
           (view (new-view player current-level scoreboard)))

;;;;;;;;;;;;;;;;;;; DESTRUCTIVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (define (next-level!) ;; Update variables and return next level
        (scoreboard 'save-high-score)
        (if (not (null? (cdr levels)))
          (begin (set! levels (cdr levels))
                 (set! current-level (new-level player 
                                                (car levels)))
                 (view 'set-level! current-level))
          (display "GAME COMPLETED")))

;;;;;;;;;;;;;;;;;;; KEY HANDLER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (define (key-handler state key)
        (cond ((eq? state 'pressed)
               (case key
                 ((up down left right) (if (current-level 'is-legal-move? player key)
                                         ((player 'get-position) 'move! key)
                                         (current-level 'try-opening! player key)))))))

;;;;;;;;;;;;;;;;;;; GAME LOOP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (define (game-loop ms)
        (if (not (current-level 'is-finished? player))
          (begin (current-level 'update! ms) ; Update all the models
                 (view 'update! ms) ; Update all the views
                 (current-level 'clear-updates!) ; Clean up updates list after updating
                 (if (player 'is-dead?)
                   (current-level 'respawn))) 
          (next-level!)))

;;;;;;;;;;;;;;;;;;; START LOOP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ((view 'key-handler) key-handler)
      ((view 'game-loop) game-loop)))
       

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (define (dispatch cmd . args)
        (cond ((eq? cmd 'start) (apply start args))
              (else (error "Unknown command" cmd))))

      dispatch)
