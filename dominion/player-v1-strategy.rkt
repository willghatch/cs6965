#lang racket/base

(require "player.rkt")
(parameterize ([current-buy-filter v1-filter])
  (play-loop) )
