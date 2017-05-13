#lang racket/base

(require
  (only-in racket/undefined undefined)
  (only-in syntax/readerr raise-read-error))

(provide parendown-readtable-handler)


(define (until-fn condition body)
  (unless (condition)
    (body)
    (until-fn condition body)))

(define-syntax-rule (until condition body ...)
  (until-fn (lambda () condition) (lambda () body ...)))


(define parendown-readtable-handler
  (case-lambda
    [ [ch in]
      (define-values [line col pos] (port-next-location in))
      (define src (object-name in))
      (read-list in src line col pos #f)]
    [ [ch in src line col pos]
      (read-list in src line col pos #t)]))

(define (read-list in src line col pos should-read-syntax)
  (define rev-elems (list))
  (define next-char undefined)
  (until
    (begin
      (regexp-match #px"^\\s*" in)
      (set! next-char (peek-char in))
      (when (eq? eof next-char)
        (define span 2)
        (raise-read-error
          "read: expected `)', `]', or `}' to close `#/'"
          src line col pos span))
      (memq next-char (list #\) #\] #\})))
    (define elem
      (if should-read-syntax
        (read-syntax/recursive src in)
        (read/recursive in)))
    (unless (special-comment? elem)
      (set! rev-elems (cons elem rev-elems))))
  (define result (reverse rev-elems))
  (define-values [stop-line stop-col stop-pos]
    (port-next-location in))
  (when should-read-syntax
    (set! result
      (datum->syntax #f result
        (and line
          (vector src line col pos (- stop-pos pos)))))
    (when (eq? next-char #\])
      (set! result (syntax-property result 'paren-shape #\[)))
    (when (eq? next-char #\})
      (set! result (syntax-property result 'paren-shape #\{))))
  result)
