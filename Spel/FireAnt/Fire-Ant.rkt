(#%require (only racket random))
(#%require "lib/Graphics.rkt")

(load "controller/Game.rkt")

(define game (new-game))
((game 'start))
