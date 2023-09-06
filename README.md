# timer/timer

Interval timers from [Racket] threads

A [Racket] module that provides:

- `call-with-timer-semaphore`
- `join-timer`
- `kill-timer`
- `start-timer`
- `timer?`
- `timer-thread`
- `timer-semaphore`

Timer objects are created using `start-timer`:

```scheme
(define my-timer (start-timer 3000.0
                              (lambda (message)
                                  (display message) (newline))
                              #f
                              "expired"))
```

The preceding will write `expired` followed by a newline to the
standard output port in a separate thread after at least three seconds
have elapsed. The returned timer object can be used to further
interact with the timer's thread. Note that for compatibility with
Racket's built in `call-with-semaphore`, `start-timer` takes an
optional `try-fail-handler` thunk and a _rest_ argument that can be
used to parameterize the procedure invoked by the worker thread. In
this case, however, the `try-fail-handler` will never be invoked
because the semaphore will be created and immediately locked by the
calling thread before even creating the worker thread. This might
change in some future version of this library module if the need
arises to support passing in an existing semaphore.

Use `timer?` to determine whether or not any given value is a timer:

```scheme
(timer? my-timer) ; => #t
(timer? 42) ; => #f
```

Each timer object is an association between a semaphore and a worker
thread. The worker thread will invoke the procedure passed to
`start-timer` with the semaphore locked after at least the specified
number of milliseconds have elapsed. The thread is created with the
semaphore initially locked by the calling thread until `start-timer`
returns. It is then re-locked by the worker thread when the timer
expires for the duration of the call to the timer-handler procedure.

The `call-with-timer-semaphore` procedure can be used by other threads
to execute code with the same semaphore locked as that used by a
timer's worker thread.

```scheme
(define my-result (call-with-timer-semaphore
                      my-timer
                      (lambda (message)
                          (display message)
                          (newline)
                          42)
                      #f
                      "in main thread"))
my-result ; => 42
```

Like `start-timer`, `call-with-timer-semaphore` implements a contract
identical to that of Racket's built-in `call-with-semaphore` other
than the data type of the first argument. Unlike `start-timer`, the
`try-fail-handler` argument to `call-with-timer-semaphore` is
effective to guard against possible race conditions between the
calling thread and the given timer's worker thread.

Use `join-timer` to block the calling thread until a given timer's
worker thread terminates:

```scheme
(join-timer my-timer)
```

Use `kill-timer` to forcibly terminate a timer whether or not it has
yet expired:

```scheme
(kill-timer my-timer)
```

For completeness, the following timer struct accessors are exported:

``` scheme
(timer-semaphore my-timer) ; => semaphore
(timer-thread my-timer) ; => thread
```

Beware of using the struct accessors in ways that might break the
timer encapsulation.

## Pre-Requisites

Working _Racket_ installation:
<https://docs.racket-lang.org/getting-started/index.html>

## Building

```bash
git clone git@github.com:parasaurolophus/racket-timer.git
cd racket-timer
raco pkg install --name timer --link $PWD
```

## Examples

### Racket

```scheme
#lang racket
; import the timer/timer module
(require timer/timer)

; start a timer that writes "timer expired" to stdout after 3 seconds
(define timer (start-timer 3000.0 (lambda () (display "expired\n"))))

; show that timer? returns true for an object returned by start-timer
(display (timer? timer))
(newline)

; write "in main thread" to stdout with the timer semaphore locked
(call-with-timer-semaphore
    timer
    (lambda (message)
        (display message)
        (newline))
    #f
    "in main thread")

; kill the timer thread
(kill-timer timer)
```

### R<sup>6</sup>RS

The main reason this module exists is to provide a convenience wrapper
around native Racket features, packaged in a way that makes them
accessible from other languages supported by the Racket runtime. For
example, to define the preceding in R<sup>6</sup>RS Scheme, simply
replace the first three lines in the above with:

```scheme
#!r6rs
; import the compatibility library and timer/timer modules
(import (rnrs) (timer timer))
```

The rest of the example is already R<sup>6</sup>RS compatible. You can
use `start-timer`, `call-with-timer-semaphore` etc. even though you
will not be able to access the underlying thread and semaphore
primitives directly.

[Racket]: https://racket-lang.org/
