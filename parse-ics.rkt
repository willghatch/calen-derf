#lang racket/base

(require "parse-ics-content-lines.rkt")
(require racket/list)
(require racket/string)
(require racket/match)
(require kw-make-struct)

;; note - property parameter values are case insensitive unless they are in quotes,
;;        and they should never contain quotes


;; TODO - make cline->string, v<whatever>->string, line-limit xformer to round-trip back to ics

(define (cline-name-matcher name)
  (λ (cline) (and (content-line? cline)
                  (equal? (content-line-name cline)
                          name))))
(define (car-maybe l)
  (if (pair? l)
      (car l)
      #f))

(define (filter-pred/left pred l)
  (let ([passes (filter pred l)])
    (values passes (filter (λ (x) (not (member x passes))) l))))
(define (filter-name/left part-name l)
  (filter-pred/left (cline-name-matcher part-name) l))

(define begin? (cline-name-matcher "BEGIN"))
(define end? (cline-name-matcher "END"))

(define (treeify-content-lines lines)
  (define (inner end-tag lines-left)
    (if (empty? lines-left)
        (values empty empty)
        (let ([l (first lines-left)]
              [r (rest lines-left)])
          (cond 
            [(and (end? l) (equal? (content-line-value l) end-tag))
             (values empty r)]
            [(end? l) (error "unexpected END token with tag ~a" content-line-value l)]
            [(begin? l)
             (let ([new-tag (content-line-value l)])
               (let-values ([(ret1 new-left) (inner new-tag r)])
                 (let-values ([(ret2 newer-left) (inner end-tag new-left)])
                   (values (cons (cons new-tag ret1) ret2) newer-left))))]
            [else (let-values ([(ret new-left) (inner end-tag r)])
                    (values (cons l ret) new-left))]))))
  (let-values ([(ret left) (inner #f lines)])
    (if (empty? left)
        ret
        (error "Treeifying the ICS failed for some reason.  This is a bug."))))

(define (wrap-with-begin-end-str lines-str tag)
  (format "~a~a~a"
          (content-line->string (content-line "BEGIN" '() tag))
          lines-str
          (content-line->string (content-line "END" '() tag))))

(define (vobj->string obj)
  ;; Well this would be a nice place for generic functions or OO...
  (cond [(content-line? obj) (content-line->string obj)]
        [(vcalendar? obj) (vcalendar->string obj)]
        [(vevent? obj) (vevent->string obj)]
        [(valarm? obj) (valarm->string obj)]
        [(vtodo? obj) (vtodo->string obj)]
        [(vjournal? obj) (vjournal->string obj)]
        [(vunknown? obj) (vunknown->string obj)]
        [(list? obj) (vobj-list->string obj)]
        [(not obj) ""]))
(define (vobj-list->string l)
  (string-join (map vobj->string l) ""))

(struct vcalendar
  (prod-id version method events todos journals other-parts)
  #:transparent)
(define (stuff-vcalendar parts)
  (let*-values
      ([(fp) filter-pred/left]
       [(fn) filter-name/left]
       [(left) parts]
       [(prod-ids left) (fn "PRODID" left)]
       [(versions left) (fn "VERSION" left)]
       [(methods left) (fn "METHOD" left)]
       [(events left) (fp vevent? left)]
       [(todos left) (fp vtodo? left)]
       [(journals left) (fp vjournal? left)])
    (vcalendar (car-maybe prod-ids) (car-maybe versions) (car-maybe methods)
               events todos journals left)))
(define (vcalendar->string o)
  (match o
    [(make/kw vcalendar
              #:prod-id prod-id
              #:version version
              #:method method
              #:events events
              #:todos todos
              #:journals journals
              #:other-parts other-parts)
     (wrap-with-begin-end-str
      (string-append (vobj->string prod-id)
                     (vobj->string version)
                     (vobj->string method)
                     (vobj->string events)
                     (vobj->string todos)
                     (vobj->string journals)
                     (vobj->string other-parts)
                     )
      "VCALENDAR")]))

(struct vevent
  (
   start
   end
   summary
   description
   location
   alarms

   timestamp
   created-time
   last-modified-time

   organizers
   attendees
   uid
   sequence
   status

   other-parts)
  #:transparent)
#;(define (vevent/kw #:start [start #f]
                   #:end [end #f]
                   #:summary [summary #f]
                   #:description [description #f]
                   #:location [location #f]
                   #:alarms [alarms #f]
                   #:timestamp [timestamp #f]
                   #:created-time [created-time #f]
                   #:last-modified-time [last-modified-time #f]
                   #:organizers [organizers #f]
                   #:attendees [attendees #f]
                   #:uid [uid #f]
                   #:sequence [sequence #f]
                   #:status [status #f]
                   #:other-clines [other-clines #f]
                   )
  (vevent start end summary description location alarms timestamp created-time
          last-modified-time organizers attendees uid sequence status other-clines))

(define (stuff-vevent parts)
  (let*-values
      ([(f) filter-name/left]
       [(p) filter-pred/left]
       [(starts left) (f "DTSTART" parts)]
       [(ends left) (f "DTEND" left)]
       [(timestamps left) (f "DTSTAMP" left)]
       [(last-modifieds left) (f "LAST-MODIFIED" left)]
       [(created-times left) (f "CREATED" left)]

       [(uids left) (f "UID" left)]

       [(organizers left) (f "ORGANIZER" left)]
       [(attendees left) (f "ATTENDEE" left)]
       [(summaries left) (f "SUMMARY" left)]
       [(descriptions left) (f "DESCRIPTION" left)]
       [(locations left) (f "LOCATION" left)]
       [(sequence-nums left) (f "SEQUENCE" left)]
       [(statuses left) (f "STATUS" left)]
       [(alarms left) (p valarm? left)]
       )
    ;; TODO -error checking on fields that should only have 1 element, etc
    (make/kw vevent
             #:start (car-maybe starts)
             #:end (car-maybe ends)
             #:summary (car-maybe summaries)
             #:description (car-maybe descriptions)
             #:location (car-maybe locations)
             #:alarms alarms

             #:timestamp (car-maybe timestamps)
             #:created-time (car-maybe created-times)
             #:last-modified-time (car-maybe last-modifieds)

             #:organizers (car-maybe organizers)
             #:attendees attendees
             #:uid (car-maybe uids)

             #:sequence (car-maybe sequence-nums)
             #:status (car-maybe statuses)
             #:other-parts left
             )))
(define (vevent->string o)
  (match o
    [(make/kw vevent
              #:start start
              #:end end
              #:summary summary
              #:description description
              #:location location
              #:alarms alarms
              #:timestamp timestamp
              #:created-time created-time
              #:last-modified-time last-modified-time
              #:organizers organizers
              #:attendees attendees
              #:uid uid
              #:sequence sequence
              #:status status
              #:other-parts other-parts
              )
     (wrap-with-begin-end-str
      (string-append (vobj->string start)
                     (vobj->string end)
                     (vobj->string summary)
                     (vobj->string description)
                     (vobj->string location)
                     (vobj->string alarms)
                     (vobj->string timestamp)
                     (vobj->string created-time)
                     (vobj->string last-modified-time)
                     (vobj->string organizers)
                     (vobj->string attendees)
                     (vobj->string uid)
                     (vobj->string sequence)
                     (vobj->string status)
                     (vobj->string other-parts)
                     )
      "VEVENT")]))

(struct valarm
  (trigger description action other-parts)
  #:transparent)
(define (stuff-valarm parts)
  (let*-values
      ([(fp) filter-pred/left]
       [(fn) filter-name/left]
       [(triggers left) (fn "TRIGGER" parts)]
       [(descriptions left) (fn "DESCRIPTION" left)]
       [(actions left) (fn "ACTION" left)])
    (valarm (car-maybe triggers) (car-maybe descriptions) (car-maybe actions) left)))
(define (valarm->string o)
  (let ([desc (vobj->string (valarm-description o))]
        [trigger (vobj->string (valarm-trigger o))]
        [action (vobj->string (valarm-action o))]
        [misc (vobj->string (valarm-other-parts o))])
    (wrap-with-begin-end-str (string-append desc trigger action misc) "VALARM")))

(struct vtodo
  (other-parts)
  #:transparent)
(define (stuff-vtodo parts)
  (vtodo parts))
(define (vtodo->string o)
  (let ([str (vobj->string (vtodo-other-parts o))])
    (wrap-with-begin-end-str str "VTODO")))

(struct vjournal
  (other-parts)
  #:transparent)
(define (stuff-vjournal parts)
  vjournal parts)
(define (vjournal->string o)
  (let ([str (vobj->string (vjournal-other-parts o))])
    (wrap-with-begin-end-str str "VJOURNAL")))

(struct vunknown
  (tag parts)
  #:transparent)
(define (stuff-vunknown tag parts)
  (vunknown tag parts))
(define (vunknown->string o)
  (let ([str (vobj->string (vunknown-parts o))])
    (wrap-with-begin-end-str str (vunknown-tag o))))


(define (treeified-cont-lines->vcal-objects tree)
  ;; this tree always is a list at the top level, probably of one item
  (for/list ([o tree])
    (if (list? o)
        (let* ([tag (first o)]
               [innards (rest o)]
               [xformed-innards (treeified-cont-lines->vcal-objects innards)])
          (case tag
            [("VCALENDAR") (stuff-vcalendar xformed-innards)]
            [("VEVENT") (stuff-vevent xformed-innards)]
            [("VALARM") (stuff-valarm xformed-innards)]
            [("VTODO") (stuff-vtodo xformed-innards)]
            [else (stuff-vunknown tag xformed-innards)]))
        o)))


(module+ main
  (for ([arg (current-command-line-arguments)])
    (for ([item (treeified-cont-lines->vcal-objects
                 (treeify-content-lines
                  (ics->content-lines
                   (open-input-file arg))))])
      (display (vobj->string item))
      )))
