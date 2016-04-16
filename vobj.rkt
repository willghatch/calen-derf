#lang racket/base

(require "content-line.rkt")
(require "cl-datetime.rkt")
(require racket/list)
(require racket/string)
(require racket/match)
(require kw-make-struct)
(require syntax/parse/define)
(require (for-syntax syntax/parse))
(require (for-syntax racket/syntax))
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
(define (date->cl/curry name)
  (λ (d) (date->cl d name)))
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


(define-syntax-parser def-vobj
  [(def-vobj vobj-name:id
     begin/end-tag-string:str
     ([part-name:id matcher cline-> ->cline req/opt-spec] ...)
     extras-name:id)
   (with-syntax ([stuffer-name (format-id #'vobj-name "stuff-~a" #'vobj-name)]
                 [to-clines-name (format-id #'vobj-name "~a->content-lines" #'vobj-name)]
                 [to-string-name (format-id #'vobj-name "~a->string" #'vobj-name)]
                 [(accessor ...)
                  (datum->syntax
                   #'vobj-name
                   (map (λ (n) (format-id #'vobj-name "~a-~a" #'vobj-name n))
                        (syntax->list #'(part-name ...))))]
                 [extras-accessor (format-id #'vobj-name "~a-~a" #'vobj-name #'extras-name)])
     #`(begin
         (struct vobj-name (extras-name part-name ...) #:transparent)
         (define (stuffer-name parts)
           (let ([part-hash
                  (build-vobj-hash
                   parts
                   (list (vobj-part-spec (quote part-name)
                                         matcher cline->
                                         req/opt-spec)
                         ...)
                   (quote extras-name))])
             (vobj-name (hash-ref part-hash (quote extras-name))
                        (hash-ref part-hash (quote part-name)) ...)))
         (define (to-clines-name o)
           (flatten
            (list (content-line "BEGIN" '() begin/end-tag-string)
                  (let* ([xf (or ->cline (λ (x) x))])
                    (if (equal? req/opt-spec 'list)
                        (map xf (accessor o))
                        (xf (accessor o))))
                  ...
                  ;;((or ->cline (λ (x) x)) (accessor o)) ...
                  (extras-accessor o)
                  (content-line "END" '() begin/end-tag-string))))
         (define (to-string-name o)
           (string-join (map vobj->string (to-clines-name o)) ""))
         ;; END def-vobj
         ))])

(def-vobj vcalendar
  ;; field, matcher/pred, xf-in, xf-out, ropt-spec
  "VCALENDAR"
  ([prod-id "PRODID" #f #f 'required]
   [version "VERSION" #f #f 'required]
   [method "METHOD" #f #f 'optional]
   [events vevent? #f vevent->content-lines 'list]
   [todos vtodo? #f vtodo->content-lines 'list]
   [journals vjournal? #f vjournal->content-lines 'list])
  extras)

(def-vobj vevent
  ;; field, matcher/pred, xf-in, xf-out, ropt-spec
  "VEVENT"
  (
   ;; required, only one
   [timestamp "DTSTAMP" cl->date (date->cl/curry "DTSTAMP") 'required]
   [uid "UID" #f #f 'required]

   ;; required unless there is a METHOD property, only one
   [start "DTSTART" cl->date (date->cl/curry "DTSTART") 'optional]

   ;; optional, only one
   ;; alternatively there can be a DURATION rather than DTEND, but only one of the two
   [end "DTEND" cl->date (date->cl/curry "DTEND") 'optional]

   [summary "SUMMARY" #f #f 'optional]
   [description "DESCRIPTION" #f #f 'optional]
   [location "LOCATION" #f #f 'optional]
   [created-time "CREATED" cl->date (date->cl/curry "CREATED") 'optional]
   [last-modified-time "LAST-MODIFIED" cl->date (date->cl/curry "LAST-MODIFIED") 'optional]
   [organizer "ORGANIZER" #f #f 'optional]
   [sequence "SEQUENCE" #f #f 'optional]
   [status "STATUS" #f #f 'optional]

   ;; optional, only one, TODO
   ;class
   ;geo
   ;priority
   ;transp
   ;url
   ;recurid

   ;; optional, SHOULD not be more than one
   ;rrule

   ;; optional, multiple times
   [attendees "ATTENDEE" #f #f 'list]
   ;attachments
   ;categories
   ;comments
   ;contacts
   ;exdate
   ;rstatus
   ;related
   ;resources
   ;rdate
   [alarms valarm? #f valarm->content-lines 'list])
  extras)

(def-vobj valarm
  ;; field, matcher/pred, xf-in, xf-out, ropt-spec
  "VALARM"
  ([trigger "TRIGGER" #f #f 'required]
   [action "ACTION" #f #f 'required]
   [description "DESCRIPTION" #f #f 'optional]
   [summary "SUMMARY" #f #f 'optional]
   [duration "DURATION" #f #f 'optional]
   [repeat "REPEAT" #f #f 'optional]
   [attach "ATTACH" #f #f 'optional]
   [attendees "ATTENDEE" #f #f 'list]
   )
  extras)

(def-vobj vtodo
  ;; field, matcher/pred, xf-in, xf-out, ropt-spec
  "VTODO"
  ()
  extras)

(def-vobj vjournal
  ;; field, matcher/pred, xf-in, xf-out, ropt-spec
  "VJOURNAL"
  ()
  extras)

(def-vobj vtimezone
  "VTIMEZONE"
  ()
  extras)
(def-vobj v-tz-standard
  "STANDARD"
  ()
  extras)
(def-vobj v-tz-daylight
  "DAYLIGHT"
  ()
  extras)
(def-vobj vfreebusy
  "VFREEBUSY"
  ()
  extras)

(def-vobj vcard
  "VCARD"
  ()
  extras)


(struct vunknown
  (extras tag)
  #:transparent)
(define (stuff-vunknown tag parts)
  (vunknown parts tag))
(define (vunknown->string o)
  (let ([str (vobj->string (vunknown-extras o))])
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
