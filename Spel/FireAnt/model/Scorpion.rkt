;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scorpion ADT is verantwoordelijk voor:
;; [ ] 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-scorpion position route)
  (let ((type 'scorpion))
;;;;;;;;;;;;;;;;;;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (get-type)
      type)

    (define (get-position)
      position)

;;;;;;;;;;;;;;;;;;; SETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; OTHER FUNC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'get-type) (apply get-type args))
            ((eq? cmd 'get-position) (apply get-position args))
            (else (error "Unkown command" cmd))))

    dispatch))
