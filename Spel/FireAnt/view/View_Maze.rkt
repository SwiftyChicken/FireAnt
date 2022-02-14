(define (new-view owner)
  (let ((layer (canvas 'make-layer)))
    (define (dispatch cmd)
      (cond ((eq? cmd 'draw) draw)))

    dispatch))
