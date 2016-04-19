#lang racket/base

(require racket/function)
(require racket/path)
(require racket/file)
(require racket/string)
(require "vobj.rkt")

(provide
 (struct-out vdir)
 (struct-out vdir-elem)
 write-vdir!
 read-vdir
 vdir-add-vobj
 )


(struct vdir
  (elems path ics/vcf))

(struct vdir-elem
  (vobjects path modified-time))

(define (write-vdir-elem! e)
  (display-to-file (string-join (map vobj->string (vdir-elem-vobjects e)) "")
                   (vdir-elem-path e)
                   #:exists 'replace))

(define (write-vdir-elem-if-newer! elem)
  (when (or (not (file-exists? (vdir-elem-path elem)))
              (> (vdir-elem-modified-time elem)
                 (file-or-directory-modify-seconds (vdir-elem-path elem))))
      (write-vdir-elem! elem)))

(define (write-vdir! vdir)
  (for ([elem (vdir-elems vdir)])
    (write-vdir-elem-if-newer! elem)))

(define (read-vdir path)
  (let ([epath (expand-user-path path)])
    (let* ([files (directory-list epath #:build? #t)]
           [elems
            (for/list ([f files])
              (with-handlers ([(λ _ #t) (λ _ #f)])
                (let* ([mod-time (file-or-directory-modify-seconds f)]
                       [vobjs (port->vobjects (open-input-file f))])
                  (vdir-elem vobjs f mod-time))))])
      (vdir (filter identity elems) path (filename-extension path)))))

(define (vdir-add-vobj vd vo)
  (cond
    [(vevent? vo)
     (let* ([uid (ep-val (vevent-uid vo))]
            [filename (string-append uid ".ics")]
            [filepath (build-path (vdir-path vd) filename)]
            [mod-time (current-seconds)]
            ;; it has to be wrapped in a vcalendar object
            [vcal (vcalendar/default
                   #:prod-id calen-derf-prod-id
                   #:version vcalendar-version-string
                   #:events (list vo))]
            [elem (vdir-elem (list vcal) filepath mod-time)])
       (struct-copy vdir vd
                    [elems (cons elem (vdir-elems vd))]))]
    [else (error 'vdir-add-vobj "Tried to add unsupported vobject to vdir.")]))

(module+ main
  (for ([vdir (current-command-line-arguments)])
    (for ([vdir-elem (vdir-elems (read-vdir vdir))])
      (for ([vo (vdir-elem-vobjects vdir-elem)])
        (displayln (vobj->string vo))))
    )

  )
