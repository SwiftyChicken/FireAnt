(load "model/Positie.rkt")

(define (new-player)
  (let ((positie (new-positie 0 0)))

    (define (dispatch cmd)
      (cond ((eq? cmd 'pos) positie)))

    dispatch))
