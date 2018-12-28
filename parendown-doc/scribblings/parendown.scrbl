#lang parendown scribble/manual

@; parendown/scribblings/parendown.scrbl
@;
@; Weak opening paren functionality in the form of a language
@; extension and a library.

@;   Copyright 2018 The Lathe Authors
@;
@;   Licensed under the Apache License, Version 2.0 (the "License");
@;   you may not use this file except in compliance with the License.
@;   You may obtain a copy of the License at
@;
@;       http://www.apache.org/licenses/LICENSE-2.0
@;
@;   Unless required by applicable law or agreed to in writing,
@;   software distributed under the License is distributed on an
@;   "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
@;   either express or implied. See the License for the specific
@;   language governing permissions and limitations under the License.


@(require #/for-label racket/base)
@(require #/for-label #/only-in racket/contract/base any/c)

@(require #/for-label parendown)


@title{Parendown}

@defmodulelang[parendown]

Parendown adds @emph{weak opening parentheses} to Racket in the form of a language extension. A more extensive overview of Parendown's uses can be found in @hyperlink["https://github.com/lathe/parendown-for-racket"]{the README at GitHub}.

@; TODO: Copy at least a little more of the readme into this blurb.



@table-of-contents[]



@section[#:tag "language-extension"]{The Parendown Language Extension}

The @tt{parendown} language is a language extension. To use it, specify another language after @tt{parendown} on the @hash-lang[] line, and that language will have its readtable extended with a @tt{#/} syntax that behaves according to @racket[parendown-readtable-handler].

@racketblock[
  @#,racketmetafont{@hash-lang[] parendown @racketmodname[racket/base]}
  
  (displayln @#,tt{#/}string-append "Hello, " "world!")
]


@section[#:tag "parendown-library"]{Parendown as a Library}

There is also a @tt{parendown} module which lets Racket code use some features of Parendown even when they aren't using the @hash-lang[] directly.

@defform[(pd slash-symbol stx ...)]{
  Expands to @racket[(stx ...)], where the lists of each @racket[stx] have been recursively traversed to transform any tails that begin with the symbol @racket[slash-symbol] into tails consisting of a single element, where that element is the list resulting from transforming the rest of that tail.
  
  For instance, the form
  
  @racketblock[
    (pd _/ begin
      (displayln _/ string-append "Hello, " "world!")
      (displayln _/ string-append "I can't use " "division!"))
  ]
  
  expands to this:
  
  @racketblock[
    (begin
      (displayln (string-append "Hello, " "world!"))
      (displayln (string-append "I can't use " "division!")))
  ]
}

@defform[
  #:link-target? #f
  (pd (stx ...))
]{
  Simply expands to @racket[(stx ...)].
  
  This is usually the result of the other case of @racket[pd]. For instance, the form
  
  @racketblock[
    (pd _/ begin
      (displayln _/ string-append "Hello, " "world!")
      (pd _/ displayln _/ string-append "I can't use " "division!"))
  ]
  
  expands to this:
  
  @racketblock[
    (begin
      (displayln (string-append "Hello, " "world!"))
      (pd (displayln (string-append "I can't use " "division!"))))
  ]
  
  This contains another occurrence of @tt{pd}, and this time, the code
  
  @racketblock[
    (pd (displayln (string-append "I can't use " "division!")))
  ]
  
  expands to this:
  
  @racketblock[
    (displayln (string-append "I can't use " "division!"))
  ]
  
  This behavior makes it so occurrences of the @tt{pd} form can be generously added wherever they're suspected to be needed, without causing conflicts with each other.
}

@defproc*[(
  [(parendown-readtable-handler [name char?] [in input-port?]) any/c]
  [
    (parendown-readtable-handler
      [name char?]
      [in input-port?]
      [src any/c]
      [line (or/c #f exact-positive-integer?)]
      [col (or/c #f exact-nonnegative-integer?)]
      [pos (or/c #f exact-positive-integer?)])
    any/c]
)]{
  A readtable handler procedure suitable for use with @racket[make-readtable]. This handler implements a syntax very similar to (if not necessarily in full parity with) the default read behavior for the characters @tt{(}, @tt{[}, and @tt{@"{"}, except that it doesn't consume the terminating @tt{)}, @tt{]}, or @tt{@"}"}.
  
  When the terminating character is @tt{]} or @tt{@"}"}, the resulting list's @tt{paren-shape} syntax property is set to @racket[#\[] or @racket[#\{], respectively.
  
  This readtable handler is sensitive to the @racket[read-accept-dot] and @racket[read-accept-infix-dot] parameters at the time the handler is invoked. This functionality of Parendown should be considered unstable, since it isn't quite the same as what @tt{(}, @tt{[}, and @tt{@"{"} do on contemporary versions of Racket. Those characters' default handlers are sensitive to the values of those parameters at the time the read is @emph{originally started}, not the time they are encountered during the read. For instance, in contemprary versions of Racket, if @racket[(read-accept-dot)] is @racket[#t] at the time @racket[read] is first called and then a custom reader syntax causes it to be set to @racket[#f], a subsequent occurrence of @tt{(} in the same read will be processed as though @racket[(read-accept-dot)] were still @racket[#t].
}
