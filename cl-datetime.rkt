#lang racket/base

(provide datetime-content-line->date
         date->datetime-content-line)

(require srfi/19)
(require racket/string)
(require "content-line.rkt")

(define time-format-str "~Y~m~dT~H~M~S")
(define (datetime-content-line->date cl)
  (let* ([str (content-line-value cl)]
         [first-pass (string->date str time-format-str)]
         [zulu-end? (string-suffix? str "Z")]
         [tz-params (content-line-get-param-values cl "TZID")]
         [tz-name (or (and zulu-end? "UTC")
                      (and (not (null? tz-params))
                           (car tz-params))
                      "")]
         ;; TODO - do this better
         [tz-offset 0])
    (struct-copy date* first-pass
                 [time-zone-offset #:parent date tz-offset]
                 [time-zone-name tz-name])))
(define (date->datetime-content-line d cline-name #:extra-params [extra-params '()])
  (let* ([tz-name (date*-time-zone-name d)]
         [utc? (equal? tz-name "UTC")]
         [z-str (if utc? "Z" "")]
         [datestr (date->string d time-format-str)]
         [tz-param (cond [utc? #f]
                         [(equal? tz-name "") #f]
                         [else (param "TZID" (list tz-name))])]
         [all-params (if tz-param
                         (cons tz-param extra-params)
                         extra-params)])
    (content-line cline-name all-params (format "~a~a" datestr z-str))))


