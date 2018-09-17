#lang parendown racket

; demo.rkt
;
; A demonstration of the weak opening paren functionality of the
; Parendown language extension.

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


(displayln "Hello, world!")
(writeln '(1 2 [7 (3 4) #/4 3 5]))
(displayln #/string-append "Hello, " "world!")
