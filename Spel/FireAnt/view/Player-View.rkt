(load "view/Character-View.rkt")

(define (new-player-view owner layer)
  (let* ((bitmap (string-append bitmap-dir "ant.png"))
         (mask (string-append mask-dir "ant.png"))
         (removed #f) ;; Removed from layer
         (character (new-character-view owner layer bitmap mask)))

;;;;;;;;;;;;;;;;;;; PREDICATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (is-removed?)
      removed)

;;;;;;;;;;;;;;;;;;; DESTRUCTIVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (remove!)
      (if (not removed)
        (begin (layer 'empty)
               (set! removed #t))))

    (define (reset!)
      (set! removed #f)
      (set! character (new-character-view owner layer bitmap mask)))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'is-removed?) (apply is-removed? args))
            ((eq? cmd 'remove!) (apply remove! args))
            ((eq? cmd 'reset!) (apply reset! args))
            (else (apply character (cons cmd args)))))
    dispatch))
