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
         (make-readtable (current-readtable) #\/ 'dispatch-macro
;         (make-readtable (current-readtable) #\/ 'terminating-macro
           parendown-readtable-handler)])
      (apply -read args))))

(define-values (-read -read-syntax -get-info)
  (make-meta-reader
    'parendown
    "language path"
    lang-reader-module-paths
    wrap-reader
    wrap-reader
    (lambda (-get-info) -get-info)))
