#lang racket/base

(require
  (only-in syntax/module-reader
    make-meta-reader
    lang-reader-module-paths)
  (only-in parendown parendown-readtable-handler))

(provide
  (rename-out
    [-read read]
    [-read-syntax read-syntax]
    [-get-info get-info]))

(define (wrap-reader -read)
  (lambda args
    (parameterize
      ([current-readtable
         ; NOTE: There are many syntaxes we could have used for this,
         ; but we're using #/ as the syntax right now. We can't really
         ; use / like Cene does, because although the commented-out
         ; code implements that, it would cause annoyances whenever
         ; using Racket's many identifiers with / in their names, such
         ; as racket/base.
         (make-readtable (current-readtable) #\/ 'dispatch-macro
;        (make-readtable (current-readtable) #\/ 'terminating-macro
           parendown-readtable-handler)])
      (apply -read args))))

(define-values [-read -read-syntax -get-info]
  (make-meta-reader
    'enda
    "language path"
    lang-reader-module-paths
    wrap-reader
    wrap-reader
    (lambda (-get-info) -get-info)))
