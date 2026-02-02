#lang racket/base

; parendown
;
; Parendown's weak opening paren functionality in the form of a
; syntax transformer rather than as a language extension.

;   Copyright 2017, 2018, 2021, 2025, 2026 The Lathe Authors
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
    (only-in syntax/parse ~and id syntax-parse)
    parendown/private/autoptic))

(provide
  pd)



(define-for-syntax (rebuild-syntax basis-stx incomplete-stx)
  (datum->syntax
    #;ctxt basis-stx
    incomplete-stx
    #;srcloc basis-stx
    #;prop basis-stx))

(define-for-syntax (identifier-would-bind? binding-id potential-usage)
  (and
    (identifier? potential-usage)
    (equal-always? (syntax-e binding-id) (syntax-e potential-usage))
    (scopes<=? binding-id potential-usage)))

(define-syntax (pd overall-stx)
  (syntax-parse overall-stx
    
    ; If the input appears to have already been processed by a
    ; surrounding `pd` form, that's fine. In that case `pd` behaves
    ; like an identity operation, having no effect.
    [ (_ {~and rest (_ ...)})
      
      #:when
      (autoptic-lists-to? overall-stx (list overall-stx #'rest))
      
      #'rest]
    
    [ (_ sample:id rest ...)
      #:when (autoptic-lists-to? overall-stx (list overall-stx))
      (define (add-to-syntax-property-list prop-name elem stx)
        (syntax-property stx prop-name
          (cons elem (or (syntax-property stx prop-name) (list)))))
      ; NOTE: This is a continuation-passing-style loop. The
      ; continuation is called `then`. The continuation-passing-style
      ; recursion of the loop is called `next`.
      (
        (lambda (then)
          (let next
            (
              [uses (list)]
              [ stx
                (rebuild-syntax overall-stx (syntax-e #'(rest ...)))]
              [then then])
            (syntax-parse stx
              [ (first . rest)
                #:when (autoptic-to? overall-stx stx)
                (if (identifier-would-bind? #'sample #'first)
                  (next (cons #'first uses) #'rest
                    (lambda (uses rest)
                      (then uses (rebuild-syntax #'first `(,rest)))))
                  (next uses #'first
                    (lambda (uses first)
                      (next uses #'rest
                        (lambda (uses rest)
                          (then uses
                            (rebuild-syntax stx
                              `(,first . ,rest))))))))]
              [_ (then uses stx)])))
        (lambda (uses processed)
          #`
          (begin
            ; We generate fake binding and usage sites just so the
            ; Check Syntax binding arrows look good in DrRacket.
            (let ()
              (define-syntax (sample stx) #'(void))
              #,@uses
              (void))
            #,processed)))]))
