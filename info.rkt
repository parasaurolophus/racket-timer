#lang info
(define collection "timer")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/timer.scrbl" ())))
(define pkg-desc "Interval timer using Racket threads")
(define version "0.1")
(define pkg-authors '(parasaurolophus))
(define license '(MIT))
