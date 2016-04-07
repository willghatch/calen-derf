#lang racket/base

(provide (struct-out content-line)
         (struct-out param)
         port->content-lines
         content-line->string
         content-line-get-param-values
         )

(require (rename-in parsack
                    [string parsack-string]))
(require racket/stream)
(require racket/string)
(require racket/list)

(struct content-line
  (name params value)
  #:transparent)
(struct param
  (name values)
  #:transparent)

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
(define $param/semicoloned (parser-compose (char #\;)
                                           (p <- $param)
                                           (return p)))

(define $content-line
  (parser-compose (name <- $content-name-str)
                  (params <- (many $param/semicoloned))
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
  ;; Some things look like I should escape semicolons, other things don't...
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
