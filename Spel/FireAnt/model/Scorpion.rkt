;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scorpion ADT is verantwoordelijk voor:
;; [ ] 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-scorpion position pathing-code)
  (let* ((type 'scorpion)
         (pathing (list->circular 
                    (code->path pathing-code)))
         (iterate (caar pathing))
         (direction (translate (cdar pathing))))

;;;;;;;;;;;;;;;;;;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (get-type)
      type)

    (define (get-position)
      position)

;;;;;;;;;;;;;;;;;;; SETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;; AUXILIARY FUNC ;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (next-command)
      (set! pathing (cdr pathing))
      (set! iterate (caar pathing))
      (set! direction (translate (cdar pathing))))
;;;;;;;;;;;;;;;;;;; OTHER FUNC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (update)
      (if (> iterate 0)
        (begin (position 'move direction)
               (set! iterate (- iterate 1)))
        (begin (next-command)
               (update))))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'get-type) (apply get-type args))
            ((eq? cmd 'update) (apply update args))
            ((eq? cmd 'get-position) (apply get-position args))
            (else (error "Unkown command" cmd))))

    (display pathing)
    dispatch)

;;;;;;;;;;;;;;;;;;; AUXILIARY FUNC ;;;;;;;;;;;;;;;;;;;;;;;;;
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

  (define (translate direction)
    (case direction
      ((#\U) 'up)
      ((#\D) 'down)
      ((#\L) 'left)
      ((#\R) 'right)
      (else (error "Unknown direction" direction)))))
