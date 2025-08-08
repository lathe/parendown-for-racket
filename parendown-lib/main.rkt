#lang racket/base

; parendown
;
; Parendown's weak opening paren functionality in the form of a
; library rather than as a language extension. (The language extension
; is implemented in terms of this.)

;   Copyright 2017-2018, 2021, 2025 The Lathe Authors
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
  (for-syntax
    racket/base
    (only-in racket/match match)
    (only-in syntax/parse id syntax-parse))
  (only-in racket/contract/base
    -> and/c any any/c case-> contract-out or/c)
  (only-in racket/undefined undefined)
  (only-in syntax/readerr raise-read-error))

(provide
  pd
  (contract-out
    [
      parendown-readtable-handler
      (case->
        (-> char? input-port? any/c)
        (->
          char?
          input-port?
          any/c
          (or/c #f exact-positive-integer?)
          (or/c #f exact-nonnegative-integer?)
          (or/c #f exact-positive-integer?)
          any/c))]
    [
      parendown-color-lexer
      (-> (and/c string? immutable?) (-> any/c any/c any)
        procedure?)]))



; ===== Weak opening brackets from a syntax transformer ==============

(define-for-syntax (rebuild-syntax basis-stx incomplete-stx)
  (datum->syntax
    #;ctxt basis-stx
    incomplete-stx
    #;srcloc basis-stx
    #;prop basis-stx))

(define-syntax (pd stx)
  (syntax-parse stx
    
    ; If the input appears to have already been processed by a
    ; surrounding `pd` form, that's fine. In that case `pd` behaves
    ; like an identity operation, having no effect.
    [(_ (rest ...)) #'(rest ...)]
    
    [ (_ sample:id rest ...)
      (define (add-to-syntax-property-list prop-name elem stx)
        (syntax-property stx prop-name
          (cons elem (or (syntax-property stx prop-name) (list)))))
      (define uses (list))
      (define processed
        (let loop ([stx (rebuild-syntax stx (syntax-e #'(rest ...)))])
          (syntax-parse stx
            [ (first . rest)
              (if
                (and
                  (identifier? #'first)
                  (bound-identifier=? #'sample #'first))
                (begin
                  (set! uses (cons #'first uses))
                  (rebuild-syntax #'first `(,(loop #'rest))))
                (rebuild-syntax stx
                  `(,(loop #'first) . ,(loop #'rest))))]
            [_ stx])))
      #`(begin
          ; We generate fake binding and usage sites just so the
          ; Check Syntax binding arrows look good in DrRacket.
          (let ()
            (define-syntax (sample stx) #'(void))
            #,@uses
            (void))
          #,processed)]))



; ===== Weak opening brackets from a reader extension ================

(define (until-fn condition body)
  (unless (condition)
    (body)
    (until-fn condition body)))

(define-syntax-rule (until condition body ...)
  (until-fn (lambda () condition) (lambda () body ...)))

; Racket's `peek-char` lets you skip a number of *bytes*, but not a
; number of characters. This one lets you skip a number of characters.
(define (peek-char-skipping-chars in skip-chars-amt)
  (let ([peeked-string (peek-string (add1 skip-chars-amt) 0 in)])
    (string-ref peeked-string skip-chars-amt)))

(define (non-terminating-char? readtable x)
  (and (char? x)
    (let ()
      (define-values
        (char-terminating char-entry char-dispatch-entry)
        (readtable-mapping readtable x))
      (or (eq? 'non-terminating-macro char-terminating)
        (and (char? char-terminating)
          (parameterize ([current-readtable #f])
            (define symbol-name (string #\a char-terminating))
            (not
              (eq? 'a (read (open-input-string symbol-name))))))))))


(define parendown-readtable-handler
  (case-lambda
    [ (name in)
      (define-values (line col pos) (port-next-location in))
      (define src (object-name in))
      (read-list name in src line col pos #f)]
    [ (name in src line col pos)
      (read-list name in src line col pos #t)]))

(define (read-list name in src line col pos should-read-syntax)
  (define span 1)
  (define (read-as-we-should [in in])
    (if should-read-syntax
      (read-syntax/recursive src in)
      (read/recursive in)))
  (define (read-skipping-comments)
    (define result (read-as-we-should))
    (until (not (special-comment? result))
      (set! result (read-as-we-should)))
    result)
  (define (skip-whitespace)
    (regexp-match #px"^\\s*" in))
  (define (like-default char . originals)
    (define-values (char-terminating char-entry char-dispatch-entry)
      (readtable-mapping (current-readtable) char))
    (memq char-terminating originals))
  
  ; NOTE: Racket doesn't provide any really elegant way to skip
  ; comments without reading the next non-comment expression... but it
  ; turns out the built-in list syntax doesn't *use* any really
  ; elegant way to do it either. So, the function
  ; `peek-after-whitespace-and-comments` here is full of ad hoc,
  ; unhygienic behavior, but almost all of it is meant to simulate
  ; specific behaviors of the built-in list syntax.
  (define (peek-after-whitespace-and-comments)
    (skip-whitespace)
    (define next-char (peek-char in))
    
    (cond
      [(eof-object? next-char) next-char]
      [ (like-default next-char #\;)
        (read-as-we-should)
        (peek-after-whitespace-and-comments)]
      [(not (like-default next-char #\#)) next-char]
      [#t
        (define-values (dispatch-line dispatch-col dispatch-pos)
          (port-next-location in))
        (define dispatch-span 2)
        (define hash-char next-char)
        (define fake-dispatch-string (peek-string 2 0 in))
        (cond
          [(< (string-length fake-dispatch-string) 2) hash-char]
          [ (like-default (string-ref fake-dispatch-string 1) #\;)
            (read-string 2 in)
            ; NOTE: Inside of the built-in list syntax, a #;(...)
            ; comment works like this even if ; has been bound to a
            ; custom `dispatch-macro` in the current readtable. Inside
            ; the expression, the readtable seems to be the same as
            ; it is on the outside, so the built-in list syntax does
            ; *not* parameterize the readtable with a default
            ; implementation of #; to do the read here.
            (read-skipping-comments)
            (peek-after-whitespace-and-comments)]
          [ (like-default (string-ref fake-dispatch-string 1) #\|)
            (read-string 2 in)
            
            ; NOTE: Inside of the built-in list syntax, a #|...|#
            ; comment isn't picky about which exact characters are
            ; used to close it, as long as they have the appropriate
            ; `readtable-mapping`. Outside of a list, a #|...|#
            ; comment is pickier than that.
            (define (read-rest-of-fake-nested-comment)
              (define first (read-char in))
              (define second (peek-char in))
              (when (or (eof-object? first) (eof-object? second))
                (raise-read-error "read: end of file in #| comment"
                  src
                  dispatch-line dispatch-col dispatch-pos
                  dispatch-span))
              
              (cond
                [ (like-default first #\#)
                  (if (like-default second #\|)
                    (begin
                      (read-char in)
                      (read-rest-of-fake-nested-comment)
                      (read-rest-of-fake-nested-comment))
                    (read-rest-of-fake-nested-comment))]
                [ (like-default first #\|)
                  (if (like-default second #\#)
                    (read-char in)
                    (read-rest-of-fake-nested-comment))]
                [#t (read-rest-of-fake-nested-comment)]))
            
            (read-rest-of-fake-nested-comment)
            (peek-after-whitespace-and-comments)]
          [#t hash-char])]))
  
  (define (closing? char)
    (like-default char #\) #\] #\}))
  
  ; TODO: The built-in list syntax actually seems to obtain the value
  ; of these parameters at the time the *original* read procedure was
  ; called, before any recursive reads. For instance, the expression
  ; (1 2 * (3 . 4) 5), where * is a custom reader macro that sets
  ; `read-accept-dot` to #f, reads as (1 2 (3 . 4) 5) if
  ; `read-accept-dot` was true before the read. See if there's a way
  ; we can simulate that.
  (define accept-dot (read-accept-dot))
  (define accept-infix-dot (read-accept-infix-dot))
  
  ; These variables are the state of the next loop.
  (define rev-elems (list))
  (define improper-tail (list))
  (define action-on-non-comment void)
  (define next-char undefined)
  (define listening-for-dots accept-dot)
  (until
    (begin
      (skip-whitespace)
      (set! next-char (peek-after-whitespace-and-comments))
      
      (when (eof-object? next-char)
        (raise-read-error
          (string-append
            "read: expected `)', `]', or `}' to close "
            "`" (string name) "'")
          src line col pos span))
      
      (if
        (and listening-for-dots
          (like-default next-char #\.)
          (not
            (non-terminating-char? (current-readtable)
              ; TODO: This is the only place we peek more than one
              ; character ahead. See if the official reader performs a
              ; read and then a peek here instead.
              (peek-char-skipping-chars in 1))))
        (let ()
          (define-values (dot-line dot-col dot-pos)
            (port-next-location in))
          (define dot-span 1)
          (define dot-char next-char)
          (define (dot-err [dot-char dot-char])
            (raise-read-error
              (string-append
                "read: illegal use of `" (string dot-char) "'")
              src dot-line dot-col dot-pos dot-span))
          
          (read-char in)
          (define elem (read-skipping-comments))
          (define possible-next-dot-or-closing
            (peek-after-whitespace-and-comments))
          (cond
            [(eof-object? possible-next-dot-or-closing) (dot-err)]
            [ (and accept-infix-dot
                (like-default possible-next-dot-or-closing #\.))
              
              (read-char in)
              
              ; NOTE: In the built-in list syntax, the syntax
              ; (1 2 . 3 . *), where * is a custom reader macro that
              ; returns a special comment, reads as (1 2). For parity,
              ; we don't add the infix operator to the list until we
              ; encounter the next non-comment list element. Note that
              ; comments like ; #; and #| are skipped a different way
              ; (emulated here by
              ; `peek-after-whitespace-and-comments`), so they don't
              ; exhibit the same behavior.
              (set! action-on-non-comment
                (lambda ()
                  (set! action-on-non-comment void)
                  (set! rev-elems (append rev-elems (list elem)))))
              
              (set! listening-for-dots #f)
              (define possible-closing
                (peek-after-whitespace-and-comments))
              (when (eof-object? possible-closing)
                ; TODO: This prints garbage to the console, but this
                ; has parity with Racket. See if this is a bug that
                ; needs to be fixed in Racket.
                (dot-err (integer->char #xFFFD)))
              (when (closing? possible-closing)
                (dot-err possible-closing))
              
              ; We've read the operator of an infix list, and there's
              ; still more to go after that, so we continue the loop.
              #f]
            [ (closing? possible-next-dot-or-closing)
              (set! improper-tail elem)
              (set! next-char possible-next-dot-or-closing)
              
              ; We've read the end of an improper list, so we exit the
              ; loop.
              #t]
            [ #t
              
              ; The usual error for (10 20 . 30 40) reads the "4" from
              ; the stream before it raises its error, so we do the
              ; same thing.
              (read-char in)
              
              (dot-err)]))
        
        ; If the next character is not a dot, or if we're not
        ; listening for dots, we exit the loop if we've reached a
        ; closing paren.
        (closing? next-char)))
    (define elem (read-as-we-should))
    (unless (special-comment? elem)
      (action-on-non-comment)
      (set! rev-elems (cons elem rev-elems))))
  (define result (append (reverse rev-elems) improper-tail))
  (define-values (stop-line stop-col stop-pos)
    (port-next-location in))
  (when should-read-syntax
    (set! result
      (datum->syntax #f result
        (and line (vector src line col pos (- stop-pos pos)))))
    (when (like-default next-char #\])
      (set! result (syntax-property result 'paren-shape #\[)))
    (when (like-default next-char #\})
      (set! result (syntax-property result 'paren-shape #\{))))
  result)


; Parendown's syntax highlighting recognizes the weak open paren as a
; `'parenthesis` token, and it passes all other processing through to
; the extended language's syntax highlighter.
;
(define (parendown-color-lexer weak-open-paren -get-info)
  (define weak-open-paren-length (string-length weak-open-paren))
  
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
