#lang racket/base

(provide (struct-out content-line)
         (struct-out param)
         parse-content-lines)

(require (rename-in parsack
                    [string parsack-string]))
(require racket/stream)

(struct content-line
  (name params value)
  #:transparent)
(struct param
  (name values)
  #:transparent)

(define newline-chars "\r\n")
(define escape-linebreak-chars " \t")
(define control-chars ;; all controls except TAB
  (apply string (for/list ([n (append (stream->list (in-range 9))
                                      (stream->list (in-range 10 #x1F))
                                      (list #x7F))])
                  (integer->char n))))
(define q-unsafe-chars (string-append control-chars "\""))
(define unsafe-chars (string-append q-unsafe-chars ";:,"))

(define $escaped-linebreak
  (try (parser-compose (parser-cons $eol (<or> $space $tab))
                       (return 'escaped-linebreak))))

(define (with-ebreaks parser)
  ;; filter out escaped-linebreak garbage
  (parser-compose (x <- parser)
                  (return (filter (λ (v) (not (equal? v 'escaped-linebreak)))
                                  x))))

#;(define $linebreak (<or> (parser-cons $eol
                                      (notFollowedBy
                                       (oneOf escape-linebreak-chars)))
                         $eof))
(define $linebreak (<or> $eol $eof))

(define (stringify char-parser)
  (parser-compose (cs <- char-parser)
                  (return (apply string cs))))

(define $safe-char (noneOf unsafe-chars))
(define $safe-chars (with-ebreaks (many (<or> $safe-char
                                              $escaped-linebreak))))
(define $qsafe-char (noneOf q-unsafe-chars))
(define $qstring (parser-compose
                  (char #\")
                  (r <- (many $qsafe-char))
                  (char #\")
                  (return r)))
(define $content-name-chars (with-ebreaks (many (<or> $alphaNum
                                                      (char #\-)
                                                      $escaped-linebreak))))
(define $content-name-str (stringify $content-name-chars))
(define $value-char (noneOf "\r\n"))

(define $bslash-char
  (parser-compose (char #\\)
                  (c <- (char #\n))
                  (return (case c
                            [(#\n) #\newline]
                            [else c]))))

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

(define (parse-content-lines in-port)
  (parse-result $content-lines in-port))


