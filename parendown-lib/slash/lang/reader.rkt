#lang racket/base

; parendown/slash/lang/reader
;
; Parendown's weak opening paren functionality in the form of a
; language extension, using a non-symbol-terminating `/` reader macro
; instead of `#/`.

;   Copyright 2017, 2018, 2021, 2026 The Lathe Authors
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
  (only-in parendown/reader
    parendown-color-lexer parendown-readtable-handler))

(provide
  (rename-out
    [-read read]
    [-read-syntax read-syntax]
    [-get-info get-info]))

(define (wrap-reader -read)
  (lambda args
    (parameterize
      (
        [
          current-readtable
          (make-readtable (current-readtable)
            #\/ 'non-terminating-macro parendown-readtable-handler)])
      
      (apply -read args))))

(define-values (-read -read-syntax -get-info)
  (make-meta-reader
    'parendown/slash
    "language path"
    lang-reader-module-paths
    wrap-reader
    wrap-reader
    (lambda (-get-info)
      (lambda (key default-value)
        (define (fallback) (-get-info key default-value))
        (case key
          [(color-lexer) (parendown-color-lexer "/" -get-info)]
          
          ; TODO: Consider providing behavior for additional extension
          ; points. See the corresponding comment in the
          ; `#lang parendown` reader module.
          
          [else (fallback)])))))
