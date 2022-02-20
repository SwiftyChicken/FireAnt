;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game ADT is verantwoordelijk voor:
;; [x] Starten van juiste level
;; [x] Initialisatie van de Player ADT
;; [ ] Updaten van de modellen
;; [ ] Updaten van de view
;; [ ] Checken voor collisions met Player ADT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(#%require compatibility/mlist (only racket
                                     directory-list
                                     path->string))

(load "model/Level.rkt")
(load "model/Player.rkt")
(load "view/View.rkt")

(define (new-game)
  (define (start)
    (let* ((player (new-player))
           (levels (map (lambda (path) ;; List all levels in level-dir
                          (string-append level-dir (path->string path)))
                        (list->mlist (directory-list level-dir))))
           (current-level (new-level player ;; Start with level 1
                                     (car levels)))
           (view (new-view player current-level)))

      (define (next-level!) ;; Update variables and return next level
        (if (not (null? (cdr levels)))
          (begin (set! levels (cdr levels))
                 (set! current-level (new-level player 
                                                (car levels)))
                 current-level)
          (display "GAME COMPLETED")))

      (define (restart-level)
        (if (not (player 'game-over?))
          (current-level 'restart)))

;;;;;;;;;;;;;;;;;;; KEY HANDLER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ((view 'key-handler) 
       (lambda (state key)
         (cond ((eq? state 'pressed)
                (case key
                  ((up down left right) (if (not (view 'is-updating? player))
                                          (current-level 'move-player key))))))))

;;;;;;;;;;;;;;;;;;; GAME LOOP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ((view 'game-loop) 
       (lambda (ms)
         (if (not (current-level 'is-finished player))
           (begin (view 'update player ms))
           (next-level!))))))

      (define (dispatch cmd . args)
        (cond ((eq? cmd 'start) (apply start args))
              (else (error "Unknown command" cmd))))

      dispatch)
