#lang racket/base

; parendown/lang/reader
;
; Parendown's weak opening paren functionality in the form of a
; language extension.

;   Copyright 2017-2018 The Lathe Authors
;
;   Licensed under the Apache License, Version 2.0 (the "License");
;   you may not use this file except in compliance with the License.
;   You may obtain a copy of the License at
;
;       http://www.apache.org/licenses/LICENSE-2.0
;
;   Unless required by applicable law or agreed to in writing,
;   software distributed under the License is distributed on an
;   "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
;   either express or implied. See the License for the specific
;   language governing permissions and limitations under the License.


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
         ;
         ; If we do change this, we should also change the place we
         ; hardcode "#/" in the color lexer below.
         ;
         (make-readtable (current-readtable) #\/ 'dispatch-macro
;         (make-readtable (current-readtable) #\/ 'terminating-macro
           parendown-readtable-handler)])
      
      (apply -read args))))

; Parendown's syntax highlighting recognizes the weak open paren as a
; `'parenthesis` token, and it passes all other processing through to
; the extended language's syntax highlighter.
;
(define (wrap-color-lexer -get-info)
  
  ; TODO: Should we check for whether `-get-info` is false before
  ; calling it here? Other languages seem to do that, but the
  ; documented contract of `make-meta-reader` specifies that it will
  ; at least be a `procedure?`, not `(or/c #f procedure?)`.
  ;
  (define get-info-fallback-color-lexer (-get-info 'color-lexer #f))
  
  (define default-fallback-color-lexer
    (if (procedure? get-info-fallback-color-lexer)
      get-info-fallback-color-lexer
      
      ; TODO: Why are we using `dynamic-require` here? Other languages
      ; do it. Is that so they can keep their package dependencies
      ; small and only depend on DrRacket-related things if the user
      ; is definitely already using DrRacket?
      ;
      ; TODO: Some languages even guard against the possibility that
      ; the packages they `dynamic-require` don't exist. Should we do
      ; that here?
      ;
      (dynamic-require 'syntax-color/racket-lexer 'racket-lexer)))
  
  (define normalized-fallback-color-lexer
    (if (procedure-arity-includes? default-fallback-color-lexer 3)
      default-fallback-color-lexer
      (lambda (in offset mode)
        (define-values (text sym paren start stop)
          (default-fallback-color-lexer in))
        (define backup-distance 0)
        (define new-mode mode)
        (values text sym paren start stop backup-distance new-mode))))
  
  (lambda (in offset mode)
    (define weak-open-paren "#/")
    (define weak-open-paren-length (string-length weak-open-paren))
    (define peeked (peek-string weak-open-paren-length 0 in))
    (if (and (string? peeked) (string=? weak-open-paren peeked))
      (let ()
        (define-values (line col pos) (port-next-location in))
        (read-string weak-open-paren-length in)
        (define text weak-open-paren)
        (define sym 'parenthesis)
        (define paren #f)
        
        ; TODO: The documentation of `start-colorer` says the
        ; beginning and ending positions should be *relative* to the
        ; original `port-next-location` of "the input port passed to
        ; `get-token`" (called `in` here), but it raises an error if
        ; we use `(define start 0)`. Is that a documentation issue?
        ; Perhaps it should say "the input port passed to the first
        ; call to `get-token`."
        ;
        (define start pos)
        (define stop (+ start weak-open-paren-length))
        
        (define backup-distance 0)
        
        ; TODO: Does it always make sense to preserve the mode like
        ; this? Maybe some color lexers would want their mode updated
        ; in a different way here (not that we can do anything about
        ; it).
        ;
        (define new-mode mode)
        
        (values text sym paren start stop backup-distance new-mode))
      (normalized-fallback-color-lexer in offset mode))))

(define-values (-read -read-syntax -get-info)
  (make-meta-reader
    'parendown
    "language path"
    lang-reader-module-paths
    wrap-reader
    wrap-reader
    (lambda (-get-info)
      (lambda (key default-value)
        (define (fallback) (-get-info key default-value))
        (case key
          [(color-lexer) (wrap-color-lexer -get-info)]
          [else (fallback)])))))
