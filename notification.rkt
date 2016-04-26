#lang racket/base

(require "vobj.rkt")
(require "event-todo-times.rkt")
(require racket/string)
(require racket/date)
(require (prefix-in srfi-19: srfi/19))
(require racket/system)
(require racket/match)
(require racket/port)

(provide
 (struct-out notification)
 mk-notification
 display-notification
 )

(struct notification
  (alarm alarm-parent effective-time-to-display)
  #:transparent)

(define (mk-notification alarm parent)
  (let* ([trigger (valarm-trigger/ne alarm)]
         [abs (valarm-trigger-struct-absolute-time trigger)]
         [rel-to (valarm-trigger-struct-related-to trigger)]
         [rel-time (valarm-trigger-struct-related-time trigger)]
         [time (or abs (get-relative-time rel-to rel-time parent))])
    (notification alarm parent time)))

(define (my-date->string d)
  (srfi-19:date->string d "~Y-~m-~d ~H:~M"))

(define (get-start/due-notification-str vo)
  (cond [(vtodo? vo)
         ;; show due date
         (let ([due (get-end-time-seconds vo)])
           (and due (format "DUE: ~a" (my-date->string (seconds->date due)))))]
        [(vevent? vo)
         ;; show start time
         (let ([start (get-start-time-seconds vo)])
           (and start (format "START: ~a" (my-date->string (seconds->date start)))))]))

(define (mk-notification-strings alarm parent)
  (let* ([alarm-strings (list (valarm-summary/ne alarm) (valarm-description/ne alarm))]
         [event-strings (list (get-start/due-notification-str parent))])
    (filter (λ(x)x) (append alarm-strings event-strings))))

(define (display-notification notification)
  (let ([strs (mk-notification-strings (notification-alarm notification)
                                       (notification-alarm-parent notification))])
    (notify strs)))

(define (notify strings)
  (define summary (if (null? strings)
                      "event notification with no other info..."
                      (car strings)))
  (define rest (string-join (if (null? strings) '() (cdr strings))
                            "\n"))
  (define proc-parts
    (process* (find-executable-path "notify-send")
              "--app-name=calen-derf"
              summary
              rest))
  (match proc-parts
    [(list in out pid in-err sig-proc)
     (begin
       (close-output-port out)
       (close-input-port in)
       (close-input-port in-err)
       (sig-proc 'wait))]))
