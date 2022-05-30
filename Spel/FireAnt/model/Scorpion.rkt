(load "model/Scorpion-Green.rkt")
(load "model/Scorpion-Yellow.rkt")

(define (new-scorpion color position args)
;;;;;;;;;;;;;;;;;;; DISPATCH LET ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let* ((type 'scorpion)
         (duration 0)
         (boosting #f)
         (cooldown 0)
         (on-cooldown #f)
         (scorpion-type (case color
                          ((yellow) (new-scorpion-yellow position args))
                          ((green) (new-scorpion-green position args))
                          (else (error "Unkown color" color)))))

;;;;;;;;;;;;;;;;;;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (get-type)
      type)

    (define (get-position)
      position)

    (define (get-color)
      color)

;;;;;;;;;;;;;;;;;;; DESTRUCTIVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (try-boosting! ms)
      (if (not (position 'is-moving?))
        (if boosting
          (begin (set! duration (+ duration ms))
                 (if (> duration 500)
                
                   (begin (set! duration 0)
                          (set! on-cooldown #t)
                          (set! color (scorpion-type 'get-race))
                          (position 'reset-speed!))))
          (if on-cooldown
            (begin (set! cooldown (+ cooldown ms))
                   (if (> cooldown 100)
                     (begin (set! on-cooldown #f)
                            (set! cooldown 0))))
            (begin (if (coin-flip 1)
                     (begin (set! boosting #t)
                            (set! color 'purple)
                            (position 'increase-speed!))))))))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'get-type) (apply get-type args))
            ((eq? cmd 'get-position) (apply get-position args))
            ((eq? cmd 'get-color) (apply get-color args))
            ((eq? cmd 'try-boosting!) (apply try-boosting! args))
            (else (apply scorpion-type (cons cmd args)))))
    dispatch))
