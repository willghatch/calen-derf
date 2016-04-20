#lang racket/base

(provide content-line->p-date
         p-date->content-line
         ical-datetime-str->date
         date->ical-datetime-str
         )

(require srfi/19)
(require racket/string)
(require "content-line.rkt")

(define time-format-str "~Y~m~dT~H~M~S")

(define (ical-datetime-str->date str #:tz-name [tz-name #f])
  (let* ([first-pass (string->date str time-format-str)]
         [zulu-end? (string-suffix? str "Z")]
         [tz-name (or (and zulu-end? "UTC")
                      tz-name
                      "")]
         ;; TODO - get the actual offset based on the name
         [tz-offset 0])
    (struct-copy date* first-pass
                 [time-zone-offset #:parent date tz-offset]
                 [time-zone-name tz-name])))
(define (date->ical-datetime-str d)
  (let* ([tz-name (date*-time-zone-name d)]
         [utc? (equal? tz-name "UTC")]
         [z-str (if utc? "Z" "")]
         [datestr (date->string d time-format-str)])
    (format "~a~a" datestr z-str)))

(define (content-line->date cl)
  (if (not cl)
      #f
      (let* ([str (content-line-value cl)]
             [tz-params (content-line-get-param-values cl "TZID")]
             [tz-name (or (and (not (null? tz-params))
                               (car tz-params))
                          "")])
        (ical-datetime-str->date str #:tz-name tz-name))))
(define (content-line->p-date cl)
  (eparams (content-line-filter-out-params cl '("TZID"))
           (content-line->date cl)))

(define (date->cl-transformer d)
  (let* ([tz-name (date*-time-zone-name d)]
         [utc? (equal? tz-name "UTC")]
         [tz-params (cond [utc? '()]
                          [(equal? tz-name "") '()]
                          [else (list (param "TZID" (list tz-name)))])])
    (values tz-params (date->ical-datetime-str d))))

(define (p-date->content-line tag pd)
  (->content-line tag pd #:transformer date->cl-transformer))

