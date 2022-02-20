;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scorpion ADT is verantwoordelijk voor:
;; [ ] 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-scorpion position unprocessed-path)
  (define (code->path code) ;; Pathing Code consist of 2 digits followed by 1 letter
    (if (not (eq? (modulo (length code) 3)
                  0))
      (error "Expected length of list to be divisible by 3" code))

    (if (null? code)
      '()
      (let ((num (string->number (string (car code) 
                                            (cadr code))))
            (char (caddr code)))
        (cons (cons num char) (code->path (cdddr code))))))

  (let ((type 'scorpion)
        (pathing (list->circular 
                   (code->path unprocessed-path))))

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

    (display pathing)
    dispatch))
