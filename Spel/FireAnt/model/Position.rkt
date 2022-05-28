;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Position ADT is verantwoordelijk voor:
;; [x] Heeft een type die opgevraagd kan worden
;; [x] Onthoudt zijn x en y positie, oriëntatie, snelheid en of hij beweegt van positie
;; [x] Checken van collisies met een andere positie
;; [x] Zijn buur posities kunnen terug geven 
;; [x] Verplaatsen van een positie wanneer mogelijk
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-position pos-x pos-y)
  (define normal-speed 0.17)
  (define high-speed 0.25)
  (let ((type 'position)
        (position (cons pos-x pos-y))
        (orientation #f)
        (speed normal-speed) ;; The speed at which the corresponding view should move the tile
        (moving #f)) ;; Checked if the corresponding view is still moving the tile

;;;;;;;;;;;;;;;;;;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (get-type)
      type)

    (define (get-x)
      (car position))

    (define (get-y)
      (cdr position))

    (define (get-orientation)
      orientation)

    (define (get-speed)
      speed)

;;;;;;;;;;;;;;;;;;; SETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (set-x! new-x)
      (set-car! position new-x))

    (define (set-y! new-y)
      (set-cdr! position new-y))

    (define (set-orientation! direction)
      (set! orientation direction))

    (define (set-moving! bool)
      (set! moving bool))

    (define (increase-speed!)
      (set! speed high-speed))

    (define (reset-speed!)
      (set! speed normal-speed))

;;;;;;;;;;;;;;;;;;; PREDICATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (is-moving?)
      moving)

    (define (is-colliding? compared)
      (define compared-type (compared 'get-type))
      (if (eq? compared-type 'position)
        (let ((com-x (compared 'get-x))
              (com-y (compared 'get-y)))
          (and (eq? (get-x) com-x)
               (eq? (get-y) com-y)))
        (error "Expected a position type but got" compared-type)))

;;;;;;;;;;;;;;;;;;; DESTRUCTIVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (move! direction) ;; Update position and orientation
      (if (not moving)
        (let ((x (get-x))
              (y (get-y)))
          (case direction
            ((up) (set-y! (- y 1))
                  (set! orientation 'up))
            ((down) (set-y! (+ y 1))
                    (set! orientation 'down))
            ((left) (set-x! (- x 1))
                    (set! orientation 'left))
            ((right) (set-x! (+ x 1))
                     (set! orientation 'right)))
          (set-moving! #t))))

;;;;;;;;;;;;;;;;;;; NON-DESTRUCTIVE ;;;;;;;;;;;;;;;;;;;;;;;;
    (define (peek direction) ;; Give neighbouring position
      (let ((x (get-x))
            (y (get-y)))
        (case direction
          ((up) (set! y (- y 1)))
          ((down) (set! y (+ y 1)))
          ((left) (set! x (- x 1)))
          ((right) (set! x (+ x 1))))
        (new-position x y)))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'get-type) (apply get-type args))
            ((eq? cmd 'get-x) (apply get-x args))
            ((eq? cmd 'get-y) (apply get-y args))
            ((eq? cmd 'get-orientation) (apply get-orientation args))
            ((eq? cmd 'get-speed) (apply get-speed args))
            ((eq? cmd 'set-x) (apply set-x! args))
            ((eq? cmd 'set-y) (apply set-y! args))
            ((eq? cmd 'set-moving!) (apply set-moving! args))
            ((eq? cmd 'set-orientation!) (apply set-orientation! args))
            ((eq? cmd 'increase-speed!) (apply increase-speed! args))
            ((eq? cmd 'reset-speed!) (apply reset-speed! args))
            ((eq? cmd 'is-moving?) (apply is-moving? args))
            ((eq? cmd 'is-colliding?) (apply is-colliding? args))
            ((eq? cmd 'move!)(apply move! args))
            ((eq? cmd 'peek)(apply peek args))
            (else (error "Unknown command" cmd))))

    dispatch))
