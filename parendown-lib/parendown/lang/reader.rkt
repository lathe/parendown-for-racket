#lang racket/base

(require
  (for-meta 1 racket/base)
  (only-in syntax/module-reader
    make-meta-reader
    lang-reader-module-paths)
  (only-in racket/undefined undefined))

(provide
  (rename-out
    [-read read]
    [-read-syntax read-syntax]
    [-get-info get-info]))

(define (wrap-reader -read)
  (lambda args
    (parameterize ([current-readtable (-make-readtable)])
      (apply -read args))))

(define-values [-read -read-syntax -get-info]
  (make-meta-reader
    'enda
    "language path"
    lang-reader-module-paths
    wrap-reader
    wrap-reader
    (lambda (-get-info) -get-info)))

(define (-make-readtable)
  ; NOTE: There are many syntaxes we could have used for this, but
  ; we're using #/ as the syntax right now. We can't really use / like
  ; Cene does, because although the commented-out code implements
  ; that, it would cause annoyances whenever using Racket's many
  ; identifiers with / in their names, such as racket/base.
  (make-readtable (current-readtable) #\/ 'dispatch-macro
;  (make-readtable (current-readtable) #\/ 'terminating-macro
    (case-lambda
      [ [ch in]
        (define-values [line col pos] (port-next-location in))
        (define src (object-name in))
        (read-list in src line col pos #f)]
      [ [ch in src line col pos]
        (read-list in src line col pos #t)])))

(define (until-fn condition body)
  (unless (condition)
    (body)
    (until-fn condition body)))

(define-syntax (until stx)
  (syntax-case stx ()
    [ (_ condition body)
      #'(until-fn (lambda () condition) (lambda () body))]))

(define (read-list in src line col pos should-read-syntax)
  (define rev-result (list))
  (define next-char undefined)
  (until
    (begin
      (regexp-match #px"^\\s*" in)
      (set! next-char (peek-char in))
      (memq next-char (list #\) #\] #\})))
    (set! rev-result
      (cons
        (if should-read-syntax
          (read-syntax src in)
          (read in))
        rev-result)))
  (define result (reverse rev-result))
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