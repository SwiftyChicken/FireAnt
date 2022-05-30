(load "view/Item-View.rkt")

(define (new-key-view owner layer)
  (let* ((bitmap (string-append bitmap-dir "key.png"))
         (mask (string-append mask-dir "key.png"))
         (item-view (new-item-view owner layer bitmap mask)))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (apply item-view (cons cmd args)))

    dispatch))
