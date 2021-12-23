#lang racket/base

; parendown/lang/reader
;
; Parendown's weak opening paren functionality in the form of a
; language extension.

;   Copyright 2017-2018, 2021 The Lathe Authors
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
  (only-in parendown
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
          
          ; NOTE: There are many syntaxes we could have used for this,
          ; but we're using `#/`. Using `/` like Cene does would be
          ; riskier, because many symbols in Racket conain `/` in
          ; their names. Nevertheless, we the commented-out code in
          ; the alternative language `#lang parendown/slash`. It
          ; requires us to put whitespace between a Parendown weak
          ; opening paren `/` and any preceding symbol, but we've been
          ; using whitespace like that anyway.
          ;
          ; A change to this code should coincide with a change to the
          ; hardcoded `"#/"` string in the `color lexer` case below.
          ;
          (make-readtable (current-readtable)
            #\/ 'dispatch-macro parendown-readtable-handler)])
;            #\/ 'non-terminating-macro parendown-readtable-handler)])
      
      (apply -read args))))

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
          [(color-lexer) (parendown-color-lexer "#/" -get-info)]
          
          ; TODO: Consider having `#lang parendown` and
          ; `#lang parendown/slash` provide behavior for the following
          ; other extension points:
          ;
          ;   drracket:indentation
          ;     - Determining the number of spaces to indent a new
          ;       line by. For Parendown, it would be nice to indent
          ;       however the base language indents, but counting the
          ;       weak opening paren as an opening parenthesis (so
          ;       that the new line ends up indented further than a
          ;       preceding weak opening paren).
          ;
          ;   drracket:keystrokes
          ;     - Determining actions to take in response to
          ;       keystrokes. For Parendown, it might be nice to make
          ;       it so that when a weak opening paren is typed at the
          ;       beginning of a line (with some amount of
          ;       indentation), the line is reindented to be flush
          ;       with a preceding normal or weak opening paren).
          ;
          ;   configure-runtime
          ;     - Initializing the Racket runtime for executing a
          ;       Parendown-language module directly or interacting
          ;       with it at a REPL. For Parendown, it might be nice
          ;       to let the weak opening paren be used at the REPL.
          ;       Then again, will that modify the current readtable
          ;       in a way people don't expect when they run a module
          ;       directly? Also, for this to work, we need to have
          ;       Parendown attach a `'module-language` syntax
          ;       property to the module's syntax somewhere. Is it
          ;       possible to do that while also passing through the
          ;       base language's `'module-language` declaration?
          ;
          ;   drracket:submit-predicate
          ;     - Determining whether a REPL input is complete. For
          ;       Parendown, if we're supporting weak opening parens
          ;       at the REPL, we should just make sure inputs with
          ;       weak opening parens are treated as we expect. We
          ;       might not need to extend this.
          ;
          ;   module-language
          ;     - Is this the right place to look for this key? It's a
          ;       key to the `#:info` specification for
          ;       `#lang syntax/module-reader`, but maybe that's not
          ;       related. Other places in the documentation that talk
          ;       about `'module-language` are referring to a syntax
          ;       property.
          
          [else (fallback)])))))
