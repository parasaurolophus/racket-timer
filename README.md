# timer/timer

Interval timers from [Racket](https://racket-lang.org/) threads

| Procedure                   | Description                                                                                    |
|-----------------------------|------------------------------------------------------------------------------------------------|
| `start-timer`               | Invoke a thunk in a separate thread after at least a given number of milliseconds have elapsed |
| `call-with-timer-semaphore` | Invoke a thunk in the current thread with the semaphore associated with a given timer locked    |
| `kill-timer`                | Kill a timer thread                                                                            |
| `timer?`                    | Return `#t` if and only if a given object is a timer                                           |

## Usage

### Racket

```scheme
#lang racket
; import the timer/timer module
(require timer/timer)

; start a timer that writes "timer expired" to stdout after 3 seconds
(define timer (start-timer 3000.0 (lambda () (display "expired\n"))))

; show that timer? returns true for an object returned by start-timer
(timer? timer) => #t

; write "in main thread" to stdout with the timer semaphore locked
(call-with-timer-semaphore timer (lambda () (display "in main thread\n")))

; kill the timer thread
(kill-timer timer)
```

### R<sup>6</sup>RS

Replace the first three lines in the above with:

```scheme
#!r6rs
; import the compatibility library and timer/timer modules
(import (rnrs) (timer timer))
```

## Pre-Requisites

Working _Racket_ installation: <https://docs.racket-lang.org/getting-started/index.html>

## Building

```bash
git clone git@github.com:parasaurolophus/racket-timer.git
cd racket-timer
raco pkg update --link $PWD
```
