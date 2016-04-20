#lang racket/base

(require "file-formats.rkt")
(require "vobj.rkt")
(require racket/cmdline)
(require (prefix-in r: racket/date))
(require srfi/19)

(module+ main

  ;; required
  (define vdir-path #f)
  (define date-text #f)
  (define summary-text #f)
  ;; optional
  (define time-text "00:00")
  (define desc-text #f)
  (define location-text "")

  (command-line
   ;;#:program "calen-derf-add-event"
   ;;#:once-any
   #:once-each
   ;; required
   [("--vdir") path "path to vdir for calendar" (set! vdir-path path)]
   [("--date") d "date for the event" (set! date-text d)]
   [("--summary") d "summary/name for the event" (set! summary-text d)]
   ;; optional
   [("--time") d "time for the event" (set! time-text d)]
   [("--desc") d "description for the event" (set! desc-text d)]
   [("--location") d "location for the event" (set! location-text d)]
   )

  (when (not (and vdir-path date-text summary-text))
    (eprintf "Bad command, try --help (--vdir, --date, and --summary required)~n")
    (exit 1))

  (define date-no-time (r:date->seconds (string->date date-text "~Y-~m-~d")))
  (define time-lax (string->date time-text "~H:~M"))
  (define time-seconds (+ (* 60 (date-minute time-lax))
                          (* 60 60 (date-hour time-lax))))
  (define datetime (seconds->date (+ date-no-time time-seconds) #f))

  (define event
    (vevent/default
     #:uid (generate-uid)
     #:timestamp (seconds->date (current-seconds) #f)
     #:created-time (seconds->date (current-seconds) #f)
     #:last-modified-time (seconds->date (current-seconds) #f)
     #:start datetime
     #:summary summary-text
     #:description desc-text
     #:location location-text
     ))

  (define vd-blank (vdir '() vdir-path #f))
  (define vd-added (vdir-add-vobj vd-blank event))
  (write-vdir! vd-added)

  
  )
