#lang racket

;; Copyright Kirk Rader 2023

(require scribble/srcdoc
         (for-doc scribble/base scribble/manual))

(provide
 (proc-doc/names
  call-with-timer-semaphore
  (timer? procedure? . -> . any)
  (timer thunk)
  ("Invoke " (racket thunk) " with the semaphore associated with "
             (racket timer) " locked")))
(define (call-with-timer-semaphore timer thunk)
  (call-with-semaphore (timer-semaphore timer) thunk))

(provide
 (proc-doc/names
  kill-timer
  (timer? . -> . void?)
  (timer)
  ("Kill the " (racket thread) " running " (racket timer))))
(define (kill-timer timer)
  (kill-thread (timer-thread timer)))

(provide
 (proc-doc/names
  start-timer
  ((and/c real? positive?) procedure? . -> . timer?)
  (milliseconds thunk)
  ("Invoke " (racket thunk) " in a separate thread after at least "
             (racket milliseconds) " have elasped.")))
(define (start-timer milliseconds thunk)
  (let ((s (make-semaphore 1)))
    (call-with-semaphore
     s
     (lambda ()
       (let ((t (thread
                 (lambda ()
                   (sleep (/ milliseconds 1000.0))
                   (call-with-semaphore s thunk)))))
         (timer t s))))))

(provide
 (proc-doc/names
  timer?
  (any/c . -> . boolean?)
  (v)
  ("Return " (racket #t) " if and only if " (racket v) " is a timer object")))
(struct timer
  ;; Associate a thread and semaphore.
  (thread
   semaphore))
