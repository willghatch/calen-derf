#lang racket/base

(require "content-line.rkt")
(require "cl-datetime.rkt")
(require racket/list)
(require racket/string)
(require racket/match)
(require kw-make-struct)
(require syntax/parse/define)
(require (for-syntax syntax/parse))
(require (for-syntax racket/base))

;; note - property parameter values are case insensitive unless they are in quotes,
;;        and they should never contain quotes


(define (cline-name-matcher name)
  (λ (cline) (and (content-line? cline)
                  (equal? (content-line-name cline)
                          name))))
(define (car-maybe l)
  (if (pair? l)
      (car l)
      #f))
(define (date->cl d name)
  (if d (date->datetime-content-line d name) #f))
(define (cl->date cl)
  (if cl (datetime-content-line->date cl) #f))

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

(struct vobj-part-spec
  (field-name ;; field name for final hash
   match-predicate ;; to match content-lines or vobjs
   transformer ;; transform to any sort of object desired
   req/opt/list
   ;; 'required for required specs
   ;; 'optional if it is optional but only one is allowed
   ;; 'list if it is repeatable
   ))
(define (list-spec? spec)
  (equal? (vobj-part-spec-req/opt/list spec) 'list))
(define (req-spec? spec)
  (equal? (vobj-part-spec-req/opt/list spec) 'required))
(define (opt-spec? spec)
  (equal? (vobj-part-spec-req/opt/list spec) 'optional))
(define (hash-list-cons hash key val)
  (let ([l (hash-ref hash key)])
    (hash-set hash key (cons val l))))

(define (build-vobj-hash vobjs specs no-match-tag)
  (define (matched-spec-process ht spec vobj)
    (let* ([tag (vobj-part-spec-field-name spec)]
           [transformer (or (vobj-part-spec-transformer spec) (λ (x) x))]
           [rol (vobj-part-spec-req/opt/list spec)]
           [repeatable? (equal? rol 'list)]
           [taken? (and (not repeatable?) (hash-ref ht tag))])
      (cond [taken? (eprintf "Warning: non-repeatable field repeated: ~a~n" vobj)]
            [repeatable? (hash-list-cons ht tag (transformer vobj))]
            [else (hash-set ht tag (transformer vobj))])))

  (let* ([non-list-part-names (map vobj-part-spec-field-name
                                   (filter (λ (spec) (or (req-spec? spec)
                                                         (opt-spec? spec)))
                                           specs))]
         [list-part-names (map vobj-part-spec-field-name (filter list-spec? specs))]
         [hash1 (apply hash (apply append (map (λ (name) (list name '()))
                                               (cons no-match-tag list-part-names))))]
         [hash2 (foldl (λ (name hash-so-far) (hash-set hash-so-far name #f))
                       hash1
                       non-list-part-names)]
         [pre-reversed
          (for/fold ([ht hash2])
                    ([vobj vobjs])
            (let ([spec (for/or ([spec specs])
                          (let* ([pre-pred (vobj-part-spec-match-predicate spec)]
                                 [pred (if (string? pre-pred)
                                           (cline-name-matcher pre-pred)
                                           pre-pred)])
                            (if (pred vobj)
                                spec
                                #f)))])
              (if spec
                  (matched-spec-process ht spec vobj)
                  (hash-list-cons ht no-match-tag vobj))))])
    (for/fold ([ht pre-reversed])
              ([name list-part-names])
      (hash-set ht name (reverse (hash-ref ht name))))))

(define-syntax-parser mk-stuffer
  [(mk-stuffer struct-id:id
               ([part-name:keyword pred transformer rol] ...)
               rest-name:keyword)
   #`(λ (parts)
       (let ([part-hash
              (build-vobj-hash
               parts
               (list (vobj-part-spec (quote part-name) pred transformer rol) ...)
               (quote rest-name))])
         (flatten-syntax-after-4
          (make/kw struct-id
                   rest-name (hash-ref part-hash (quote rest-name))
                   (part-name (hash-ref part-hash (quote part-name))) ...
                   ))))])
(define-syntax-parser flatten-syntax-after-4
  ;; TODO - There has to be a better way to do this...
  [(fsa4 (mk/kw s-id r-n r-n-ref part-pair ...))
   (let* ([pairs (map syntax->list (syntax->list #'(part-pair ...)))]
          [pairs-appended (apply append pairs)])
     #`(mk/kw s-id r-n r-n-ref #,@pairs-appended))])

(struct vcalendar
  (prod-id version method events todos journals other-parts)
  #:transparent)
(define stuff-vcalendar
  (mk-stuffer vcalendar
              ([#:prod-id "PRODID" #f 'required]
               [#:version "VERSION" #f 'required]
               [#:method "METHOD" #f 'optional]
               [#:events vevent? #f 'list]
               [#:todos vtodo? #f 'list]
               [#:journals vjournal? #f 'list]
               )
              #:other-parts))
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
   ;; required, only one
   timestamp
   uid

   ;; required unless there is a METHOD property, only one
   start

   ;; optional, only one
   end ;; alternatively there can be a DURATION rather than DTEND, but only one of the two
   summary
   description
   location
   created-time
   last-modified-time
   organizer
   sequence
   status
   ;; optional, only one, TODO
   ;class
   ;geo
   ;priority
   ;transp
   ;url
   ;recurid

   ;; optional, should not be more than one
   ;rrule

   ;; optional, multiple times
   attendees
   ;attachments
   ;categories
   ;comments
   ;contacts
   ;exdate
   ;rstatus
   ;related
   ;resources
   ;rdate
   ;x-prop
   alarms

   other-parts)
  #:transparent)

(define stuff-vevent
  (mk-stuffer vevent
              ([#:timestamp "DTSTAMP" cl->date 'required]
               [#:uid "UID" #f 'required]

               [#:start "DTSTART" cl->date 'optional]
               [#:end "DTEND" cl->date 'optional]

               [#:summary "SUMMARY" #f 'optional]
               [#:description "DESCRIPTION" #f 'optional]
               [#:location "LOCATION" #f 'optional]
               [#:created-time "CREATED" cl->date 'optional]
               [#:last-modified-time "LAST-MODIFIED" cl->date 'optional]
               [#:organizer "ORGANIZER" #f 'optional]
               [#:sequence "SEQUENCE" #f 'optional]
               [#:status "STATUS" #f 'optional]

               [#:attendees "ATTENDEE" #f 'list]
               [#:alarms valarm? #f 'list]
               )
              #:other-parts))
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
              #:organizer organizer
              #:attendees attendees
              #:uid uid
              #:sequence sequence
              #:status status
              #:other-parts other-parts
              )
     (wrap-with-begin-end-str
      (string-append (vobj->string (date->cl start "DTSTART"))
                     (vobj->string (date->cl end "DTEND"))
                     (vobj->string summary)
                     (vobj->string description)
                     (vobj->string location)
                     (vobj->string alarms)
                     (vobj->string (date->cl timestamp "DTSTAMP"))
                     (vobj->string (date->cl created-time "CREATED"))
                     (vobj->string (date->cl last-modified-time "LAST-MODIFIED"))
                     (vobj->string organizer)
                     (vobj->string attendees)
                     (vobj->string uid)
                     (vobj->string sequence)
                     (vobj->string status)
                     (vobj->string other-parts)
                     )
      "VEVENT")]))

(struct valarm
  (
   ;; Once only
   trigger
   action ;; AUDIO, EMAIL, or DISPLAY
   description ;; for display, email
   summary ;; for email
   duration ;; time between repeats
   repeat ;; integer number of repeats
   attach ;; for audio to play

   ;; multiple
   attendees

   other-parts)
  #:transparent)
(define stuff-valarm
  (mk-stuffer valarm
              ([#:trigger "TRIGGER" #f 'required]
               [#:action "ACTION" #f 'required]
               [#:description "DESCRIPTION" #f 'optional]
               [#:summary "SUMMARY" #f 'optional]
               [#:duration "DURATION" #f 'optional]
               [#:repeat "REPEAT" #f 'optional]
               [#:attach "ATTACH" #f 'optional]
               [#:attendees "ATTENDEE" #f 'list]
               )
              #:other-parts))
(define (valarm->string o)
  (let (
        [trigger (vobj->string (valarm-trigger o))]
        [action (vobj->string (valarm-action o))]
        [desc (vobj->string (valarm-description o))]
        [summary (vobj->string (valarm-summary o))]
        [repeat (vobj->string (valarm-repeat o))]
        [attach (vobj->string (valarm-attach o))]
        [attendees (vobj->string (valarm-attendees o))]
        [misc (vobj->string (valarm-other-parts o))]
        )
    (wrap-with-begin-end-str (string-append trigger action desc
                                            summary repeat
                                            attach attendees
                                            misc)
                             "VALARM")))

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

;; TODO - other vobjects - vtimezone, vfreebusy, vcard
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
                  (port->content-lines
                   (open-input-file arg))))])
      (display (vobj->string item))
      )))
