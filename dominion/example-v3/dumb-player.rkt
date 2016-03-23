#! /home/mflatt/proj/plt/racket/bin/racket
#lang racket

(let loop ()
  (match (read)
    [`(move ,state)
     (write '(clean copper))
     (flush-output)]
    [`(moved ,who ,what)
     (void)])
  
  (loop))
