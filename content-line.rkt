#lang racket/base

(provide
 (struct-out content-line)
 (struct-out param)
 mkparam
 (struct-out eparams)
 ep-val
 port->content-lines
 content-line->string
 content-line->eparams-string
 content-line-get-param-values
 content-line-filter-out-params
 ->content-line
 $param-list

 duration-string->seconds
 seconds->duration-string
 )

(require (rename-in parsack
                    [string parsack-string]))
(require racket/stream)
(require racket/string)
(require racket/list)

(module+ test
  (require rackunit)
  )

(struct content-line
  ;; string, list of params, string
  (name params value)
  #:transparent)
(struct param
  ;; string, list of strings
  (name values)
  #:transparent)
(define (mkparam name . vals)
  (param name vals))

(struct eparams
  ;; eparams is a wrapper for transformed content lines.
  ;; My idea is that I should always be able to parse the whole value
  ;; into some better object, but there may always be extra parameters
  ;; that I've ignored.  So this will store them.
  (params value)
  #:transparent)
(define (ep-val v)
  (if (eparams? v)
      (eparams-value v)
      v))

(define (->content-line tag value
              ;; transformer should be (x -> (values '(param ...) value-string))
              ;; the default transformer is good for basic string values
              #:transformer [transform (λ (str) (values '() str))])
  ;; This is a generic transformer to get to content lines
  (cond
    [(not value) #f]
    [(content-line? value) value]
    [(eparams? value)
     (let-values ([(params str-val) (transform (eparams-value value))])
       (content-line tag (append params (eparams-params value)) str-val))]
    [else (let-values ([(params str-val) (transform value)])
            (content-line tag params str-val))]))

(define (content-line->eparams-string cl #:string-> [string-> (λ (x) x)])
  (if (not (content-line? cl))
      #f
      (eparams (content-line-params cl)
               (content-line-value (string-> cl)))))

(define escape-linebreak-chars " \t")
(define control-chars ;; all controls except TAB
  (apply string (for/list ([n (append (stream->list (in-range 9))
                                      (stream->list (in-range 10 #x1F))
                                      (list #x7F))])
                  (integer->char n))))
(define q-unsafe-chars (string-append control-chars "\"\\"))
(define unsafe-chars (string-append q-unsafe-chars ";:,"))

(define $escaped-linebreak
  (try (parser-compose (parser-cons $eol (<or> $space $tab))
                       (return 'escaped-linebreak))))

(define (with-ebreaks parser)
  ;; filter out escaped-linebreak garbage
  (parser-compose (x <- parser)
                  (return (filter (λ (v) (not (equal? v 'escaped-linebreak)))
                                  x))))

(define $linebreak (<or> $eol $eof))

(define (stringify char-parser)
  (parser-compose (cs <- char-parser)
                  (return (apply string cs))))

(define $bslash-char
  (try (parser-compose (char #\\)
                       (c <- $anyChar)
                       (return (case c
                                 [(#\n) #\newline]
                                 [else c])))))

(define $safe-char (noneOf unsafe-chars))
(define $safe-chars (with-ebreaks (many (<or> $safe-char
                                              $bslash-char
                                              $escaped-linebreak))))
(define $qsafe-char (noneOf q-unsafe-chars))
(define $qstring (parser-compose
                  (char #\")
                  (r <- (with-ebreaks (many (<or> $qsafe-char
                                                  $bslash-char
                                                  $escaped-linebreak))))
                  (char #\")
                  (return r)))
(define $content-name-chars (with-ebreaks (many (<or> $alphaNum
                                                      (char #\-)
                                                      $escaped-linebreak))))
(define $content-name-str (stringify $content-name-chars))
(define $value-char (noneOf "\r\n\\"))

(define $value-chars (with-ebreaks (many (<or> $value-char
                                               $escaped-linebreak
                                               $bslash-char))))
(define $value-str (stringify $value-chars))


(define $param-value-chars
  (<or> $safe-chars $qstring))
(define $param-value-str (stringify $param-value-chars))
(define $param-values (sepBy1 $param-value-str (char #\,)))
(define $param (parser-compose (name <- $content-name-str)
                               (char #\=)
                               (vals <- $param-values)
                               (return (param name vals))))
(define $param-list (sepBy $param (char #\;)))
(define $param/semicoloned (parser-compose (char #\;)
                                           (plist <- $param-list)
                                           (return plist)))

(define $content-line
  (parser-compose (name <- $content-name-str)
                  (params <- (<or> $param/semicoloned (return '())))
                  (char #\:)
                  (val <- $value-str)
                  $linebreak
                  (return (content-line name params val))))

(define $content-lines (many $content-line))

(define (port->content-lines in-port)
  (parse-result $content-lines in-port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; to-string

(define (escape-newlines str)
  (string-replace str "\n" "\\n"))
(define (escape-semi str)
  (string-replace str ";" "\\;"))
(define (escape-bslash str)
  (string-replace str "\\" "\\\\"))
(define (escape-quote str)
  (string-replace str "\"" "\\\""))
(define (cline-val-escape str)
  ;; Some ics files I see have semicolon and comma escaped in values,
  ;; but SOME values need to NOT have them escaped -- eg. RRULE, which
  ;; has param-like info in the value field.  So I think never escaping
  ;; them is the right choice.
  (escape-newlines (escape-bslash str)))
(define (param-val-dquote-maybe str)
  (if (or (string-contains? str ";")
          (string-contains? str ":")
          (string-contains? str "\n")
          (string-contains? str "\"")
          (string-contains? str ","))
      (string-append "\""
                     (escape-newlines (escape-quote (escape-bslash str)))
                     "\"")
      str))
(define (content-line-param-values->string values)
  (string-join (map param-val-dquote-maybe values) ","))
(define (content-line-params->string params)
  (string-join (map (λ (p) (format ";~a=~a"
                                   (param-name p)
                                   (content-line-param-values->string
                                    (param-values p))))
                    params)
               ""))

(define (ics-line-wrap str)
  ;; The spec says that implementations SHOULD wrap lines to not be longer
  ;; than 75 characters.
  (define line-length 75)
  (define (inner str len)
    (if {(string-length str) . > . len}
        (string-append (substring str 0 len)
                       "\r\n "
                       ;; all lines after the first will be prefixed with a space,
                       ;; so make it one character shorter.
                       (inner (substring str len) (sub1 line-length)))
        str))
  (inner str line-length))

(define (content-line->string cline #:wrap? [wrap? #t])
  ;; The spec specifies specifically the use of \r\n newlines...
  ;; so I guess I will use them, despite the fact that I hate them.
  (let ([cline-str
         (format "~a~a:~a\r\n"
                 (content-line-name cline)
                 (content-line-params->string (content-line-params cline))
                 (cline-val-escape (content-line-value cline)))])
    (if wrap?
        (ics-line-wrap cline-str)
        cline-str)))


(define (content-line-get-param-values cline param-id)
  ;; returns value list for given parameter name
  (let* ([params (content-line-params cline)]
         ;; I think parameters shouldn't be duplicated, but I'm not 100% sure.
         [right-params (filter (λ (p) (equal? (param-name p) param-id))
                               params)])
    (flatten (map param-values right-params))))

(define (content-line-filter-out-params cline exclude-param-ids)
  (let* ([params (content-line-params cline)])
    (filter (λ (p) (not (member (param-name p) exclude-param-ids)))
            params)))

(define $dur-sign
  (<or> (parser-compose (char #\-) (char #\P) (char #\T) (return -1))
        (parser-compose (char #\P) (char #\T) (return 1))))

(define $duration-part
  (parser-compose
   (ns <- (many $digit))
   (hms <- (<or> (char #\H) (char #\M) (char #\S)))
   (return (let ([mult (cond [(equal? hms #\H) (* 60 60)]
                             [(equal? hms #\M) 60]
                             [else 1])])
             (* mult (or (string->number (apply string ns)) 0))))))

(define $duration-str
  (parser-compose
   (sign <- $dur-sign)
   (durs <- (many $duration-part))
   (return (* sign (apply + durs)))))

(define (duration-string->seconds ds)
  (with-handlers ([(λ _ #t) (λ _ 0)])
    (parse-result $duration-str ds)))
(define (seconds->duration-string s)
  (let* ([as (abs s)]
         [sign (negative? s)]
         [hours (quotient as (* 60 60))]
         [minutes (quotient (remainder as (* 60 60)) 60)]
         [seconds (remainder as 60)])
    (string-append (if sign "-" "")
                   "PT"
                   (if (> hours 0) (format "~aH" hours) "")
                   (if (> minutes 0) (format "~aM" minutes) "")
                   (if (> seconds 0) (format "~aS" seconds) ""))))

(module+ test
  (check-equal? (duration-string->seconds "PT1H2M3S")
                (+ (* 60 60 1) (* 2 60) 3))
  (check-equal? (duration-string->seconds "-PT1H2M3S")
                (- (+ (* 60 60 1) (* 2 60) 3)))
  (check-equal? (seconds->duration-string (- (+ (* 60 60 1) (* 2 60) 3)))
                "-PT1H2M3S")
  (check-equal? (seconds->duration-string (+ (* 60 60 1) (* 2 60) 3))
                "PT1H2M3S")
  )
