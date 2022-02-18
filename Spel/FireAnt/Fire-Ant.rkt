(#%require (only racket random error))
(#%require "lib/Graphics.rkt")

(load "etc/paths.rkt")
(load "etc/functions.rkt")
(load "controller/Game.rkt")

(define game (new-game))
(game 'start)
