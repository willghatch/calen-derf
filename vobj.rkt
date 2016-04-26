#lang racket/base

(require "content-line.rkt")
(require "cl-datetime.rkt")
(require racket/list)
(require racket/string)
(require racket/match)
(require racket/function)
(require kw-make-struct)
(require syntax/parse/define)
(require (for-syntax syntax/parse))
(require (for-syntax racket/syntax))
(require (for-syntax racket/base))

(module+ test
  (require rackunit)
  )

(provide
 port->vobjects
 vobj->string
 generate-uid
 calen-derf-prod-id
 vcalendar-version-string
 (struct-out valarm-trigger-struct)
 ;; also provided are vobjects and their /default constructors
 (all-from-out "content-line.rkt")
 )

;; note - property parameter values are case insensitive unless they are in quotes,
;;        and they should never contain quotes

(define calen-derf-prod-id "-//calen-derf//calen-derf v0//EN")
(define vcalendar-version-string "2.0")

(define (left-pad desired-length pad-char str)
  ;; Did somebody say "killer micro-library"?
  (let* ([len (string-length str)]
         [padding (make-string (- desired-length len) pad-char)])
    (string-append padding str)))

(define (generate-uid)
  (define (gen-4)
    (left-pad 4 #\0 (number->string (random 1 #xffff) 16)))
  (string-append "CD-"
                 (number->string (current-seconds) 16)
                 "-"
                 (gen-4)
                 (gen-4)))

(define (cline-name-matcher name)
  (λ (cline) (and (content-line? cline)
                  (equal? (content-line-name cline)
                          name))))
(define (car-maybe l)
  (if (pair? l)
      (car l)
      #f))
(define cl->eps content-line->eparams-string)
(define (cl->eps/t string->)
  (λ (cl) (cl->eps cl #:string-> string->)))
(define (->cl/t tag transform)
  (λ (x) (->content-line tag x #:transformer transform)))

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

(define-for-syntax (alternate-lists a b)
  (if (null? a)
      null
      (cons (car a) (cons (car b) (alternate-lists (cdr a) (cdr b))))))

(define-syntax-parser def-vobj
  [(def-vobj vobj-name:id
     begin/end-tag-string:str
     ([part-name:id matcher cline-> ->cline req/opt-spec] ...)
     extras-name:id)
   (with-syntax*
     ([stuffer-name (format-id #'vobj-name "stuff-~a" #'vobj-name)]
      [to-clines-name (format-id #'vobj-name "~a->content-lines" #'vobj-name)]
      [to-string-name (format-id #'vobj-name "~a->string" #'vobj-name)]
      [(accessor ...)
       (datum->syntax
        #'vobj-name
        (map (λ (n) (format-id #'vobj-name "~a-~a" #'vobj-name n))
             (syntax->list #'(part-name ...))))]
      [extras-accessor (format-id #'vobj-name "~a-~a" #'vobj-name #'extras-name)]
      [(accessor/ne ...)
       (datum->syntax #'vobj-name
                      (map (λ (n) (format-id #'vobj-name "~a/ne" n))
                           (syntax->list #'(accessor ...))))]
      [make-default-name (format-id #'vobj-name "~a/default" #'vobj-name)]
      [(make-default-arg-kw ...)
       (datum->syntax #'vobj-name
                      (map (λ (x) (string->keyword (symbol->string x)))
                           (syntax->datum #'(part-name ...))))]
      [(make-default-arg-name ...)
       (datum->syntax #'vobj-name (map (λ (n) (format-id #'vobj-name "~a-expr" n))
                                       (syntax->list #'(part-name ...))))]
      [(make-default-arg-form ...)
       (datum->syntax #'vobj-name
                      (map (λ (n ro-spec)
                             (let* ([ro-spec-sym (syntax->datum ro-spec)]
                                    [default-arg
                                      (case ro-spec-sym
                                        [('required) n]
                                        [('optional) `[,n #f]]
                                        [('list) `[,n '()]])])
                               (datum->syntax #'vobj-name default-arg)))
                           (syntax->list #'(make-default-arg-name ...))
                           (syntax->list #'(req/opt-spec ...))))]
      [(make-default-arg ...)
       (datum->syntax #'vobj-name
                      (alternate-lists (syntax->list #'(make-default-arg-kw ...))
                                       (syntax->list #'(make-default-arg-form ...))))]
      [make-default-extras-arg-kw
       (datum->syntax #'vobj-name
                      (string->keyword
                       (symbol->string (syntax->datum #'extras-name))))])
     #`(begin
         (struct vobj-name (extras-name part-name ...) #:transparent)
         (provide (struct-out vobj-name))
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
                  (let* ([xf (cond [(not ->cline) (λ (x) x)]
                                   [(equal? ->cline 'default)
                                    ;; the default transformer is good for strings
                                    ;; but will only work if the matcher is a string
                                    (curry ->content-line matcher)]
                                   [else ->cline])])
                    (if (equal? req/opt-spec 'list)
                        (map xf (accessor o))
                        (xf (accessor o))))
                  ...
                  (extras-accessor o)
                  (content-line "END" '() begin/end-tag-string))))
         (define (to-string-name o)
           (string-join (map vobj->string (to-clines-name o)) ""))
         (provide make-default-name)
         (define (make-default-name make-default-extras-arg-kw [extras-arg '()]
                                    make-default-arg ...)
           (vobj-name extras-arg make-default-arg-name ...))
         (define (accessor/ne o)
           ;; access parts of a vobject while unwrapping from eparams structs
           (ep-val (accessor o)))
         ...
         (provide accessor/ne) ...
         ;; END def-vobj
         ))])

(def-vobj vcalendar
  ;; field, matcher/pred, xf-in, xf-out, ropt-spec
  "VCALENDAR"
  ([prod-id "PRODID" cl->eps 'default 'required]
   [version "VERSION" cl->eps 'default 'required]
   [method "METHOD" cl->eps 'default 'optional]
   [events vevent? #f vevent->content-lines 'list]
   [todos vtodo? #f vtodo->content-lines 'list]
   [journals vjournal? #f vjournal->content-lines 'list])
  extras)

(def-vobj vevent
  ;; field, matcher/pred, xf-in, xf-out, ropt-spec
  "VEVENT"
  (
   ;; required, only one
   [timestamp "DTSTAMP"
              content-line->p-date
              (curry p-date->content-line "DTSTAMP")
              'required]
   [uid "UID" cl->eps 'default 'required]

   ;; required unless there is a METHOD property, only one
   [start "DTSTART" content-line->p-date (curry p-date->content-line "DTSTART") 'optional]

   ;; optional, only one
   ;; alternatively there can be a DURATION rather than DTEND, but only one of the two
   [end "DTEND" content-line->p-date (curry p-date->content-line "DTEND") 'optional]
   [duration "DURATION"
             (cl->eps/t duration-string->seconds)
             (->cl/t "DURATION" seconds->duration-string)
             'optional]

   [summary "SUMMARY" #f 'default 'optional]
   [description "DESCRIPTION" #f 'default 'optional]
   [location "LOCATION" #f 'default 'optional]
   [created-time "CREATED" content-line->p-date (curry p-date->content-line "CREATED") 'optional]
   [last-modified-time "LAST-MODIFIED" content-line->p-date (curry p-date->content-line "LAST-MODIFIED") 'optional]
   [organizer "ORGANIZER" #f 'default 'optional]
   [sequence "SEQUENCE" #f 'default 'optional]
   [status "STATUS" #f 'default 'optional]

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
   [attendees "ATTENDEE" #f 'default 'list]
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

(struct valarm-trigger-struct
  (absolute-time related-time related-to)
  #:transparent)
(define (cl->valarm-trigger cl)
  (let* ([related-param (content-line-get-param-values cl "RELATED")]
         [value-param (content-line-get-param-values cl "VALUE")]
         [other-params (content-line-filter-out-params cl '("RELATED" "VALUE"))]
         [relation-text (and (not (empty? related-param)) (first related-param))]
         [val-par-text (and (not (empty? value-param)) (first value-param))]
         [abs-time (and (equal? val-par-text "DATE-TIME")
                        (ical-datetime-str->date (content-line-value cl)))]
         [rel-flag (if (equal? relation-text "END") 'end 'begin)]
         [rel-amount (and (not abs-time)
                          (duration-string->seconds (content-line-value cl)))])
    (eparams other-params (valarm-trigger-struct abs-time rel-amount rel-flag))))
(define (valarm-trigger->cl-xform trig)
  (let* ([abs-time (valarm-trigger-struct-absolute-time trig)]
         [rel-time (and (not abs-time) (valarm-trigger-struct-related-time trig))]
         [value (if abs-time
                    (date->ical-datetime-str abs-time)
                    (seconds->duration-string rel-time))]
         [params (cond [abs-time (list (mkparam "VALUE" "DATE-TIME"))]
                       [(equal? (valarm-trigger-struct-related-to trig) 'end)
                        (list (mkparam "RELATED" "END"))]
                       [else '()])])
    (values params value)))
(define (valarm-trigger->cl trig)
  (->content-line "TRIGGER" trig #:transformer valarm-trigger->cl-xform))

(module+ test
  (define test-cl (content-line "TRIGGER" (list (mkparam "RELATED" "END")) "-PT15M"))
  (define test-trig (valarm-trigger-struct #f (* -1 15 60) 'end))
  (check-equal? (valarm-trigger->cl test-trig)
                test-cl)
  (check-equal? (cl->valarm-trigger test-cl)
                (eparams '() test-trig))
  )

(def-vobj valarm
  ;; field, matcher/pred, xf-in, xf-out, ropt-spec
  "VALARM"
  ([trigger "TRIGGER" cl->valarm-trigger valarm-trigger->cl 'required]
   [action "ACTION" #f 'default 'required] ;; DISPLAY, EMAIL, or AUDIO
   [description "DESCRIPTION" cl->eps 'default 'optional]
   [summary "SUMMARY" cl->eps 'default 'optional]
   [duration "DURATION"
             (cl->eps/t duration-string->seconds)
             (->cl/t "DURATION" seconds->duration-string)
             'optional]
   [repeat "REPEAT" #f 'default 'optional]
   [attach "ATTACH" #f 'default 'optional]
   [attendees "ATTENDEE" #f 'default 'list]
   )
  extras)

(def-vobj vtodo
  ;; field, matcher/pred, xf-in, xf-out, ropt-spec
  "VTODO"
  ([uid "UID" cl->eps 'default 'required]
   [timestamp "DTSTAMP"
              content-line->p-date
              (curry p-date->content-line "DTSTAMP")
              'required]
   [start "DTSTART" content-line->p-date (curry p-date->content-line "DTSTART") 'optional]
   [due "DUE" content-line->p-date (curry p-date->content-line "DUE") 'optional]
   [duration "DURATION"
             (cl->eps/t duration-string->seconds)
             (->cl/t "DURATION" seconds->duration-string)
             'optional]
   [alarms valarm? #f valarm->content-lines 'list]
   )
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
  (
   ;; version must come immediately after the BEGIN line
   [version "VERSION" #f 'default 'required]
   [prodid "PRODID" #f 'default 'optional]
   [revision "REV" content-line->p-date (curry p-date->content-line "REV") 'optional]
   [uid "UID" #f 'default 'optional]
   [formatted-names "FN" #f 'default 'list] ;; at least one is required...
   [structured-name "N" #f 'default 'optional]
   #|
   Structured name is famly-names;given-names;additional-names;honorific-prefixes;honorific-suffixes
   If you have multiple of one type of name, they are comma separated.
   |#
   [nicknames "NICKNAME" #f 'default 'list]
   [photos "PHOTO" #f 'default 'list]
   [birthday "BDAY" #f 'default 'optional]
   [anniversary "ANNIVERSARY" #f 'default 'optional]
   [gender "GENDER" #f 'default 'optional]
   [addresses "ADR" #f 'default 'list]
   #|
   address components are semicolon separated and have to be in the right order (with blanks)
   the post office box;
   the extended address (e.g., apartment or suite number);
   the street address;
   the locality (e.g., city);
   the region (e.g., state or province);
   the postal code;
   the country name

   It is recommended to leave the first two blank.

   Here's an example with properties: ADR;GEO="geo:12.3457,78.910";LABEL="Mr. John Q. Public, Esq.\n
   Mail Drop: TNE QB\n123 Main Street\nAny Town, CA  91921-1234\n
   U.S.A.":;;123 Main Street;Any Town;CA;91921-1234;U.S.A.
   |#
   [telephones "TEL" #f 'default 'list]
   [emails "EMAIL" #f 'default 'list]
   [messaging-addresses "IMPP" #f 'default 'list]
   [uris "URL" #f 'default 'list]
   [pubkeys "KEY" #f 'default 'list]
   [languages "LANG" #f 'default 'list]
   [free-busy-uris "FBURL" #f 'default 'list] ;; URLs to free-busy calendar
   [calendar-uris "CALURI" #f 'default 'list] ;; URLs to calendar
   [calendar-request-uris "CALADRURI" #f 'default 'list] ;; URLs (email addrs) for requesting appointments
   [time-zones "TZ" #f 'default 'list]
   [geocoordinates "GEO" #f 'default 'list] ;;  eg. GEO:geo:37.00,-122.00
   [titles "TITLE" #f 'default 'list]
   [roles "ROLE" #f 'default 'list]
   [relations "RELATED" #f 'default 'list]
   #|
   eg.
   RELATED;TYPE=friend:urn:uuid:f81d4fae-7dec-11d0-a765-00a0c91e6bf6
   RELATED;TYPE=contact:http://example.com/directory/jdoe.vcf
   RELATED;TYPE=co-worker;VALUE=text:Please contact my assistant Jane
   Doe for any inquiries.
   |#
   [categories "CATEGORIES" #f 'default 'list]
   #|
   tags for filtering, essentially.  This will be a list of lists in the end.
   eg.
   CATEGORIES:TRAVEL AGENT
   CATEGORIES:INTERNET,IETF,INDUSTRY,INFORMATION TECHNOLOGY
   |#

   )
  extras)


(provide (struct-out vunknown))
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

(define (port->vobjects in-port)
  (treeified-cont-lines->vcal-objects
   (treeify-content-lines
    (port->content-lines in-port))))

(module+ main
  (for ([arg (current-command-line-arguments)])
    (for ([item (treeified-cont-lines->vcal-objects
                 (treeify-content-lines
                  (port->content-lines
                   (open-input-file arg))))])
      (display (vobj->string item))))
  #;(display (vobj->string (vevent/default #:timestamp (seconds->date (current-seconds))
                                         #:start (seconds->date (current-seconds))
                                         #:uid "this-is-my-uid"
                         )))
  )
