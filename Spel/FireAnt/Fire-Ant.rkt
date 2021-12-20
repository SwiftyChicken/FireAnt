(#%require (only racket random))
(#%require "lib/Graphics.rkt")

(load "model/Game.rkt")

(define game (new-game))
((game 'start))
