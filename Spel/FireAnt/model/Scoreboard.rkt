;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Egg ADT is verantwoordelijk voor:
;; [x] Heeft een type die opgevraagd kan worden
;; [x] Onthoudt zijn positie
;; [x] Onthoudt of het egg is genomen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-scoreboard player file)
  (define (print-high-score)
    (let ((high-score ""))
      (call-with-input-file file
                          (lambda (input-port)
                            (let iter ((char (read-char input-port)))
                              (if (not (eof-object? char))
                                (begin (if (not (eq? char #\newline))
                                         (set! high-score (string-append high-score
                                                                       (string char))))
                                       (iter (read-char input-port)))))))
      (string->number high-score)))

  (let ((high-score (print-high-score))
        (changed #f)
        (level 1))

;;;;;;;;;;;;;;;;;;; PRINT TEXT FUNC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (print-score)
      (let ((score (number->string (player 'get-points))))
        (string-append "Score: \t \t" score)))

    (define (print-high-score)
      (let ((score (number->string high-score)))
        (string-append "High Score: \t" score)))

    (define (print-lives)
      (let ((lives (number->string (player 'get-lives))))
        (string-append "Extra Lives:\t# " lives)))

    (define (print-keys)
      (let ((keys (number->string (player 'get-keys))))
        (string-append "Collected Keys:\t# " keys)))

    (define (print-level)
      (let ((lvl (number->string level)))
        (string-append "Chamber:   \t# " lvl)))

;;;;;;;;;;;;;;;;;;; I/O FILE FUNC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (save-high-score) ; Overwrite file with new score if higher than old high score [etc/high-score.txt]
      (if (> current-score high-score)
        (begin (set! changed #t)
               (call-with-output-file file
                                      #:exists 'truncate
                                      (lambda (output-port)
                                        (display current-score output-port))))))

;;;;;;;;;;;;;;;;;;; PREDICATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (is-changed?) ; Do we need to redraw scoreboard?
      (or (player 'is-changed?) changed))

;;;;;;;;;;;;;;;;;;; DESTRUCTIVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (next-level!)
      (set! changed #t)
      (set! level (+ level 1)))

    (define (update!)  ; After redrawing scoreboard
      (set! changed #f)
      (player 'update!))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'print-score) (apply print-score args))
            ((eq? cmd 'print-high-score) (apply print-high-score args))
            ((eq? cmd 'print-lives) (apply print-lives args))
            ((eq? cmd 'print-keys) (apply print-keys args))
            ((eq? cmd 'print-level) (apply print-level args))
            ((eq? cmd 'save-high-score) (apply save-high-score args))
            ((eq? cmd 'is-changed?) (apply is-changed? args))
            ((eq? cmd 'next-level!) (apply next-level! args))
            ((eq? cmd 'update!) (apply update! args))
            (else (error "Unkown command" cmd))))

    dispatch))
