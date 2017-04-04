#lang racket/base

(require "vobj.rkt")
(require "cl-datetime.rkt")

(provide
 get-start-time-seconds
 get-end-time-seconds
 get-relative-time
 )

(define (get-relative-time rel-to-flag rel-time relative-to-obj)
  (let* ([rel-seconds (if (equal? rel-to-flag 'begin)
                          (get-start-time-seconds relative-to-obj)
                          (get-end-time-seconds relative-to-obj))])
    (and rel-seconds (+ rel-seconds rel-time))))

(define (get-start-time event/todo)
  (cond [(vevent? event/todo)
         (vevent-start/ne event/todo)]
        [(vtodo? event/todo)
         (vtodo-start/ne event/todo)]
        [else (error 'get-start-time "expected vevent or vtodo")]))

(define (get-start-time-seconds event/todo)
  (date->seconds/local? (get-start-time event/todo)))

(define (get-end-time-seconds/has-end event/todo)
  ;; #f if it has a duration instead
  (date->seconds/local?
   (cond [(vevent? event/todo) (vevent-end event/todo)]
         [(vtodo? event/todo) (vtodo-due event/todo)]
         [else (error 'get-end-time-seconds/has-end "expected vevent or vtodo")])))

(define (get-end-time-seconds event/todo)
  (define get-end (if (vevent? event/todo) vevent-end/ne vtodo-due/ne))
  (define get-dur (if (vevent? event/todo) vevent-duration/ne vtodo-duration/ne))
  (let* ([end (get-end event/todo)]
                [dur (get-dur event/todo)])
           (or (get-end-time-seconds/has-end event/todo)
               (let ([start (get-start-time-seconds event/todo)])
                 (and start dur (+ start dur))))))
