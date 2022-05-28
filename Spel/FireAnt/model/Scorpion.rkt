;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scorpion ADT is verantwoordelijk voor:
;; [x] Heeft een type die opgevraagd kan worden
;; [x] Onthoud zijn positie en route (stappen plan)
;; [x] Kan een gegeven path interpreteren naar een bruikbare circulaire lijst
;; [x] Updaten van zijn beweging volgens de route plan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-scorpion color position pathing-code)
;;;;;;;;;;;;;;;;;;; AUXILIARY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (interpret char)
    (case char
      ((#\U) 'up)
      ((#\D) 'down)
      ((#\L) 'left)
      ((#\R) 'right)
      (else (error "Unknown direction" direction))))
  
  (define (code->path code) 
      (if (not (eq? (modulo (length code) 3) ;; The pathing-code should consist of a repetition of 2 digits followed by 1 character
                    0))
        (error "Expected length of list to be divisible by 3" code))
  
      (if (null? code)
        '()
        (let ((num (string->number (string (car code) 
                                              (cadr code))))
              (char (caddr code)))
          (cons (cons num (interpret char)) (code->path (cdddr code))))))

;;;;;;;;;;;;;;;;;;; DISPATCH LET ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let* ((type 'scorpion)
         (pathing (list->circular 
                    (code->path pathing-code)))
         (iterate (if (eq? color 'yellow)
                    (caar pathing)
                    #f))
         (direction (if (eq? color 'yellow)
                      (cdar pathing)
                      (get-random-direction))))

;;;;;;;;;;;;;;;;;;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (get-type)
      type)

    (define (get-position)
      position)

    (define (get-color)
      color)
;;;;;;;;;;;;;;;;;;; DESTRUCTIVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (update!)
      (if (not (position 'is-moving?))
        (if (> iterate 0)
          (begin (position 'move! direction)
                 (set! iterate (- iterate 1)))
          (begin (next-command!)
                 (update!)))))

;;;;;;;;;;;;;;;;;;; AUXILIARY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (next-command!)
      (set! pathing (cdr pathing))
      (set! iterate (caar pathing))
      (set! direction (cdar pathing)))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'get-type) (apply get-type args))
            ((eq? cmd 'get-position) (apply get-position args))
            ((eq? cmd 'update!) (apply update! args))
            (else (error "Unkown command" cmd))))

    dispatch))
