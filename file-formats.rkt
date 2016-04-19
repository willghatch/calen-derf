#lang racket/base

(require racket/function)
(require racket/path)
(require "vobj.rkt")

(struct vdir
  (elems path extension))

(struct vdir-elem
  (vobjects path modified-time))

(define (write-vdir vdir)
  ;; TODO
  (void))

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


(module+ main
  (for ([vdir (current-command-line-arguments)])
    (for ([vdir-elem (vdir-elems (read-vdir vdir))])
      (for ([vo (vdir-elem-vobjects vdir-elem)])
        (displayln (vobj->string vo))))
    )

  )
