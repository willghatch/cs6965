#lang racket/base

(require "player.rkt")
(parameterize ([current-buy-filter v2-filter])
  (play-loop) )
