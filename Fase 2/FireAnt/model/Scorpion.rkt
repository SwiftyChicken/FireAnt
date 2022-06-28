(load "model/Scorpion-Green.rkt")
(load "model/Scorpion-Yellow.rkt")

(define (new-scorpion color position args)
;;;;;;;;;;;;;;;;;;; DISPATCH LET ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let* ((type 'scorpion)
         (duration 0)
         (boosting #f)
         (cooldown 0)
         (on-cooldown #f)
         (alive #t)
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

;;;;;;;;;;;;;;;;;;; PREDICATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (is-alive?)
      alive)

;;;;;;;;;;;;;;;;;;; DESTRUCTIVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (try-boosting! ms)
      (if (not (position 'is-moving?))
        (if boosting
          (begin (set! duration (+ duration ms))
                 (if (> duration BOOST-DURATION)
                   (begin (set! duration 0)
                          (set! boosting #f)
                          (set! on-cooldown #t)
                          (set! color (scorpion-type 'get-race))
                          (position 'reset-speed!)))))
          (if on-cooldown
            (begin (set! cooldown (+ cooldown ms))
                   (if (> cooldown BOOST-COOLDOWN)
                     (begin (set! on-cooldown #f)
                            (set! cooldown 0))))
            (begin (if (coin-flip BOOST-RATE)
                     (begin (set! boosting #t)
                            (set! color 'purple)
                            (position 'set-speed! FAST-SPEED)))))))

    (define (die!)
      (set! alive #f))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'get-type) (apply get-type args))
            ((eq? cmd 'get-position) (apply get-position args))
            ((eq? cmd 'get-color) (apply get-color args))
            ((eq? cmd 'is-alive?) (apply is-alive? args))
            ((eq? cmd 'try-boosting!) (apply try-boosting! args))
            ((eq? cmd 'die!) (apply die! args))
            (else (apply scorpion-type (cons cmd args)))))
    dispatch))
