;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Het constants behoud alle globale constante bij
;; Deze constanten worden aangegeven in hoofdletters voor de overzichtelijkheid
;; Deze constanten bepalen vooral de dimensies van het spel
;; Andere "globale constanten" worden anders vermeden
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define WINDOW-WIDTH 1400)
(define WINDOW-HEIGHT 880)
(define GRID-HEIGHT 20)
(define GRID-WIDTH 35)
(define TILE-SIZE 40)
(define STARTING-LIVES 3)
(define CODE-LENGTH 2)
(define BOOST-RATE 0)
(define BOOST-COOLDOWN 2000)
(define BOOST-DURATION 1000)
(define NORMAL-SPEED 0.15)
(define FAST-SPEED 0.22)
(define VERY-FAST-SPEED 0.30)
(define HIGH-SCORE-FILE (string-append score-dir "high-score.txt"))
(define DIRECTIONS (vector 'up 'right 'down 'left))
