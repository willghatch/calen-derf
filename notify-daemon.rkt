#lang racket/base
(require racket/list)
(require racket/cmdline)
(require "file-formats.rkt")
(require "vobj.rkt")
(require "notification.rkt")


#|

TODO

Recurrence rules!
Have the daemon listen for commands to add, modify, etc events.
Check modification time on vdir files and update data structure.
Make nice easy-to-edit event format to have a command call $EDITOR on a template of.

|#


(define (vdir->vobject-list vd)
  (flatten (map vdir-elem-vobjects (vdir-elems vd))))

(define (vobject->notification-list o)
  (cond [(vevent? o)
         (map (λ (a) (mk-notification a o)) (vevent-alarms o))]
        [(vtodo? o)
         (map (λ (a) (mk-notification a o)) (vtodo-alarms o))]
        [(vcalendar? o)
         (flatten (map vobject->notification-list
                       (append (vcalendar-events o)
                               (vcalendar-todos o))))]
        ;; TODO - vcard bday, other events
        ))

(define (vobject-list->notification-list objects)
  (flatten (map vobject->notification-list objects)))
(define (vdir->notification-list vd)
  (vobject-list->notification-list (vdir->vobject-list vd)))


(module+ main

  (define vdir-path #f)

  (command-line
   ;;#:program "calen-derf-add-event"
   ;;#:once-any
   #:once-each
   ;; required
   [("--vdir") path "path to vdir for calendar" (set! vdir-path path)]
   )

  (when (not vdir-path)
    (eprintf "error:  requires --vdir with path~n")
    (exit 1))


  (define init-vdir (read-vdir vdir-path))

  (define init-notification-list (vdir->notification-list init-vdir))
  (define c-s (current-seconds))
  (define future-notification-list
    (filter (λ (n) (> (notification-effective-time-to-display n)
                      c-s))
            init-notification-list))
  (define sorted-notification-list
    (sort future-notification-list
          <
          #:key notification-effective-time-to-display
          #:cache-keys? #t))

  (define (notify-or-pass-out n)
    (if (>= 40 (abs (- (notification-effective-time-to-display n)
                       (current-seconds))))
        (begin (display-notification n) #f)
        n))

  (define (notify-loop notifications)
    (define new-notifications (filter notify-or-pass-out notifications))
    (sleep 60)
    (notify-loop new-notifications))

  (notify-loop sorted-notification-list)
  )
