#lang racket/base

; parendown
;
; Parendown's weak opening paren functionality in the form of a
; library rather than as a language extension. (The language extension
; is implemented in terms of this.)

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
  (for-syntax
    racket/base
    (only-in syntax/parse id syntax-parse))
  (only-in racket/list make-list)
  (only-in racket/contract/base -> any/c case-> contract-out or/c)
  (only-in racket/port peeking-input-port input-port-append relocate-input-port))

(provide
  pd
  (contract-out
    [parendown-readtable-handler
      (case->
        (-> char? input-port? any/c)
        (->
          char?
          input-port?
          any/c
          (or/c #f exact-positive-integer?)
          (or/c #f exact-nonnegative-integer?)
          (or/c #f exact-positive-integer?)
          any/c))]))



; ===== Weak opening brackets from a syntax transformer ==============

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
        (let loop ([stx #'(rest ...)])
          (syntax-parse stx
            [ (first . rest)
              (if
                (and
                  (identifier? #'first)
                  (bound-identifier=? #'sample #'first))
                (begin
                  (set! uses (cons #'first uses))
                  #`(#,(loop #'rest)))
                #`(#,(loop #'first) . #,(loop #'rest)))]
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

(define parendown-readtable-handler
  (case-lambda
    [ (name in)
      (define-values (line col pos) (port-next-location in))
      (define src (object-name in))
      (read-list name in src line col pos #f)]
    [ (name in src line col pos)
      (read-list name in src line col pos #t)]))

(define (read-list name in src line col pos should-read-syntax)
  (define current-pos (let-values ([(line col pos) (port-next-location in)]) pos))
  (define (try opener)
    (define raw-peeking-port
      (input-port-append
       #t
       (open-input-string (apply string opener (make-list (- current-pos pos 1) #\space)))
       (peeking-input-port in)))
    (port-count-lines! raw-peeking-port)

    (define peeking-port (relocate-input-port raw-peeking-port line col pos))
    (port-count-lines! peeking-port)

    (with-handlers ([exn:fail:read? (λ (e) #f)])
      (cons (cond
              [should-read-syntax (read-syntax src peeking-port)]
              [else (read peeking-port)])
            (let-values ([(line col pos) (port-next-location peeking-port)]) pos))))

  (cond
    [(or (try #\() (try #\[) (try #\{))
     =>
     (λ (datum+pos)
       (read-string (- (cdr datum+pos) current-pos 1) in)
       (car datum+pos))]
    [else
     ;; there's a read error, so return anything and let Racket deal with the error
     #'#f]))
