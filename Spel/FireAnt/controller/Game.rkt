;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game ADT is verantwoordelijk voor:
;; [ ] Starten van juiste level
;; [ ] Intializatie van de Player ADT
;; [ ] Updaten van de modellen
;; [ ] Updaten van de view
;; [ ] Checken voor collisions met Player ADT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(#%require compatibility/mlist (only racket/base
                                     directory-list
                                     path->string))

(load "model/Level.rkt")
(load "model/Player.rkt")
(load "view/View.rkt")

(define (new-game)
  (let ((view (new-view))
        (level-dir "level"))

    (define (start)
      (let* ((levels (map (lambda (path) ;; List all levels in level-dir
                            (string-append level-dir
                                           "/"
                                           (path->string path)))
                          (list->mlist (directory-list level-dir))))
            (player (new-player))
            (current-level (new-level player ;; Start with level 1
                                      (car levels))))

        (define (next-level) ;; Update variables and return next level
          (if (not (null? (cdr levels)))
            (begin (set! levels (cdr levels))
                   (set! current-level (new-level player 
                                                  (car levels)))
                   current-level)
            (display "GAME COMPLETED")))

        ((view 'game-loop)
         (lambda (ms)
           (display current-level)
           (newline)
           (display levels)
           (newline)
           (display (next-level))
           (newline)))))

    (define (dispatch cmd)
      (cond ((eq? cmd 'start) start)))

    dispatch))
