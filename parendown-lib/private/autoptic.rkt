#lang racket/base

; autoptic.rkt
;
; A minimalistic set of utilities for checking that certain syntax
; objects are within the region where a given binding site would be
; visible, for the purpose of enforcing that the structural elements
; of a macro's syntax have been suppliied on purpose and not just
; passed through by accident from a macro template interpolation.
;
; Lathe Comforts defines slightly more advanced versions of these to
; make it more convenient to check for autopticity during a
; `syntax-parse` match. Defining our own utilities lets us avoid a
; dependency on Lathe Comforts.

;   Copyright 2025 The Lathe Authors
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


(require (only-in racket/match match))


(provide
  scopes-empty?
  scopes<=?
  autoptic-to?
  autoptic-list-to?
  autoptic-lists-to?)


(define (scopes-empty? stx)
  (bound-identifier=?
    (datum->syntax stx 'x)
    (datum->syntax #f 'x)
    0))

(define (scopes<=? a b)
  (define b-scopes-fn
    (make-syntax-delta-introducer (datum->syntax b 'x) #f 0))
  (scopes-empty? (b-scopes-fn a 'remove)))

(define (autoptic-to? surrounding-stx stx)
  (scopes<=? surrounding-stx stx))

(define (autoptic-list-to? surrounding-stx lst)
  (if (syntax? lst)
    (and (autoptic-to? surrounding-stx lst)
      (autoptic-list-to? surrounding-stx (syntax-e lst)))
    (match lst
      [(cons elem lst) (autoptic-list-to? surrounding-stx lst)]
      [(list) #t]
      [_ #f])))

(define (autoptic-lists-to? surrounding-stx lists)
  (for/and ([lst (in-list lists)])
    (autoptic-list-to? surrounding-stx lst)))
