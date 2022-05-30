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
      (print-number "Score: \t \t" (player 'get-points)))

    (define (print-high-score)
      (print-number "High Score: \t" high-score))

    (define (print-lives)
      (print-number "Extra Lives:\t# " (player 'get-lives)))

    (define (print-keys)
      (print-number "Collected Keys:\t# "(player 'get-keys)))

    (define (print-level)
      (print-number "Chamber:   \t# " level))

;;;;;;;;;;;;;;;;;;; I/O FILE FUNC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (save-high-score) ; Overwrite file with new score if higher than old high score [etc/high-score.txt]
      (let ((current-score (player 'get-points)))
        (if (> current-score high-score)
        (begin (set! changed #t)
               (set! high-score current-score)
               (call-with-output-file file
                                      #:exists 'truncate
                                      (lambda (output-port)
                                        (display current-score output-port)))))))

;;;;;;;;;;;;;;;;;;; PREDICATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (is-changed?) ; Do we need to redraw scoreboard?
      (or (player 'is-changed?) changed))

;;;;;;;;;;;;;;;;;;; DESTRUCTIVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (next-level!)
      (set! changed #t)
      (set! level (+ level 1)))

    (define (reset-level!)
      (set! changed #t)
      (set! level 1))

    (define (update!)  ; After redrawing scoreboard
      (set! changed #f)
      (player 'update!))

;;;;;;;;;;;;;;;;;;; AUXILIARY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (print-number text number)
      (let ((number (number->string number)))
        (string-append text number)))

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
            ((eq? cmd 'reset-level!) (apply reset-level! args))
            ((eq? cmd 'update!) (apply update! args))
            (else (error "Unkown command" cmd))))

    dispatch))
