(define (new-view owner canvas)
  (let ((layer (canvas 'make-layer)))
    (define (dispatch cmd)
      (cond ((eq? cmd 'draw) draw)))

    dispatch))
