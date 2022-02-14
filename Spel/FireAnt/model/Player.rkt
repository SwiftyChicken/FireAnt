(load "model/Positie.rkt")

(define (new-player)
  (let ((positie (new-positie)))

    (define (dispatch cmd)
      (cond ((eq? cmd 'pos) positie)))

    dispatch))
