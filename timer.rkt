#lang racket

;; Copyright Kirk Rader 2023

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; timer/timer
;;
;; Interval timers from Racket threads
;;
;; Provides:
;;
;; - call-with-timer-semaphore
;; - join-timer
;; - kill-timer
;; - start-timer
;; - timer?
;; - timer-semaphore
;; - timer-thread
;;
;; Timer objects are created using `start-timer`
;;
;;     (start-timer milliseconds procedure try-fail-handler arguments ...)
;;
;; Use `timer?` to determine whether or not any given value is a timer.
;;
;;     (timer? v)
;;
;; Each timer object is an association between a semaphore and a
;; worker thread. The worker thread will invoke the procedure passed
;; to `start-timer` with the semaphore locked after at least the
;; specified number of milliseconds have elapsed. The thread is
;; created with the semaphore initially locked by the calling thread
;; until `start-timer` returns. It is then re-locked by the worker
;; thread when the timer expires for the duration of the call to the
;; timer-handler procedure.
;;
;; The `call-with-timer-semaphore` procedure can be used by other
;; threads to execute code with the same semaphores locked as those
;; which are used by timer worker threads.
;;
;;     (call-with-timer-semaphore
;;         timer
;;         procedure
;;         try-fail-handler
;;         arguments ...)
;;
;; Both `start-timer` and `call-with-timer-semaphore` implement
;; contracts identical to that of Racket's built-in
;; `call-with-semaphore`, which they both invoke internally, other
;; than the data types of their first arguments. Note that the
;; `try-fail-handler` argument to `start-timer` is actually not
;; necessary since it will never be invoked in the current
;; implementation. It is provided for consistency and to future-proof
;; the contract in case it ever becomes desirable to support passing
;; in an existing semaphore when creating a timer. The
;; `try-fail-handler` argument to `call-with-timer-semaphore` is
;; effective to guard against possible race conditions between the
;; calling thread and the given timer's worker thread.
;;
;; Use `join-timer` to block the calling thread until a given timer's
;; worker thread terminates.
;;
;;     (join-timer timer)
;;
;; Use `kill-timer` to forcibly terminate a timer whether or not it
;; has yet expired.
;;
;;     (kill-timer timer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Require scribble inline documentation support modules
(require scribble/srcdoc
         (for-doc scribble/base scribble/manual))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; call-with-timer-semaphore

(provide
 (proc-doc
  call-with-timer-semaphore
  (->i ([timer timer?]
        [procedure procedure?])
       ([try-fail-thunk (or/c procedure? #f)])
       #:rest [arguments (listof any/c)]
       [result any/c])
  (#f)
  ("Apply " (racket procedure) " to the given " (racket arguments) 
            ", with the semaphore associated with " (racket timer)
            " locked")))
(define (call-with-timer-semaphore timer
                                   procedure
                                   (try-fail-thunk #f)
                                   .
                                   arguments)
  (apply call-with-semaphore (list* (timer-semaphore timer)
                                    procedure
                                    try-fail-thunk
                                    arguments)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; join-timer

(provide
 (proc-doc/names
  join-timer
  (-> timer? void?)
  (timer)
  ("Block the calling thread until the " (racket timer) " thread terminates")))
(define (join-timer timer)
  (thread-wait (timer-thread timer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; kill-timer

(provide
 (proc-doc/names
  kill-timer
  (-> timer? void?)
  (timer)
  ("Kill the " (racket thread) " running " (racket timer))))
(define (kill-timer timer)
  (kill-thread (timer-thread timer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start-timer

(provide
 (proc-doc
  start-timer
  (->i ([milliseconds (and/c real? positive?)]
        [procedure procedure?])
       ([try-fail-thunk (or/c procedure? #f)])
       #:rest [arguments (listof any/c)]
       [result any/c])
  (#f)
  ("Start a worker thread with an associated semaphore "
   "that will apply " (racket procedure) " to the given "
   (racket arguments) " after at least " (racket milliseconds)
   "have elapsed")))
(define (start-timer milliseconds procedure (try-fail-thunk #f) . arguments)
  (let ((s (make-semaphore 1)))
    (call-with-semaphore
     s
     (lambda ()
       (let ((t (thread
                 (lambda ()
                   (sleep (/ milliseconds 1000.0))
                   (apply call-with-semaphore
                          (list* s procedure try-fail-thunk arguments))))))
         (timer t s))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; timer?

(provide
 (proc-doc/names
  timer?
  (-> any/c boolean?)
  (v)
  ("Return " (racket #t) " if and only if " (racket v) " is a timer object")))
(provide
 (proc-doc/names
  timer-thread
  (-> timer? thread?)
  (timer)
  ("Return the " (racket thread) " associated with a given " (racket timer))))
(provide
 (proc-doc/names
  timer-semaphore
  (-> timer? semaphore?)
  (timer)
  ("Return the " (racket semaphore) " associated with a given "
                 (racket timer))))
(struct timer (thread semaphore))
