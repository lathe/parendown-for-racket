#lang parendown scribble/manual

@; parendown/scribblings/parendown.scrbl
@;
@; Weak opening paren functionality in the form of a language
@; extension and a library.

@;   Copyright 2018, 2021, 2025, 2026 The Lathe Authors
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
@(require #/for-label #/only-in racket/contract/base
  -> and/c any any/c)

@(require #/for-label parendown)


@title{Parendown}

Parendown adds @emph{weak opening parentheses} to Racket in the form of a language extension. A more extensive overview of Parendown's uses can be found in @hyperlink["https://github.com/lathe/parendown-for-racket"]{the README at GitHub}.

@; TODO: Copy at least a little more of the readme into this blurb.



@table-of-contents[]



@section[#:tag "language-extension"]{The Parendown Language Extension}

@defmodulelang[parendown]

The @tt{parendown} language is a language extension. To use it, specify another language after @tt{parendown} on the @hash-lang[] line. That language will have its readtable extended with a @tt{#/} syntax that behaves according to @racket[parendown-readtable-handler].

@codeblock{
  #lang parendown racket/base
  
  (displayln #/string-append "Hello, " "world!")
}


@section[#:tag "parendown/slash"]{The @racketmodname[parendown/slash] Language Extension}

@defmodulelang[parendown/slash]

The @tt{parendown/slash} language is a language extension like @tt{parendown}, but with a more streamlined syntax. To use it, specify another language after @tt{parendown/slash} on the @hash-lang[] line. That language will have its readtable extended with a @tt{/} syntax that behaves according to @racket[parendown-readtable-handler].

This acts as a non-symbol-terminating readtable extension, so symbols like @racketmodname[syntax/parse] and @racket[any/c] will be usable in the usual way. In order to make sure a @tt{/} weak opening paren isn't treated as part of the preceding symbol, it may be necessary to use whitespace in between.

@codeblock{
  #lang parendown/slash racket/base
  
  (displayln /string-append "Hello, " "world!")
}

Symbols beginning with @tt{/}, such as the division operator @racket[/], may be more difficult to use with this extension in place. However, they can still be referred to using the alternative notations @tt{\/...} and @tt{|/...|}. In the case of division, that means writing @code{\/} or @code{|/|}.


@section[#:tag "parendown-library"]{Parendown as a Library}

@defmodule[parendown #:link-target? #f]

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
  
  An occurrence of a cons cell within the @tt{pd} call syntax must have a set of scopes that's equal to or a superset of the set of scopes on the @racket[(@#,tt{pd} _...)] call itself, or that cons cell (and all its contents) will be treated as a miscellaneous datum, rather than further traversed to find @racket[slash-symbol] occurrences inside.
  
  An occurrence of @racket[slash-symbol] within the @tt{pd} call syntax must have a set of scopes that's equal to or a superset of the set of scopes on the binding occurrence of @racket[slash-symbol], or it'll be treated as a miscellaneous datum.
  
  These behaviors ensure that a @tt{pd} call that occurs within a macro's expansion template can have everyday Racket expressions interpolated into it by the template, without misinterpreting those expressions' syntactic structure.
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
  
  An occurrence of a cons cell within the @tt{pd} call syntax must have a set of scopes that's equal to or a superset of the set of scopes on the @racket[(@#,tt{pd} _...)] call itself. This behavior ensures that a @tt{pd} call that occurs within a macro's expansion template can have everyday Racket expressions interpolated into it by the template, without misinterpreting those expressions' syntactic structure.
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
  
  This readtable handler is sensitive to the @racket[read-accept-dot] and @racket[read-accept-infix-dot] parameters at the time the handler is invoked. This functionality of Parendown should be considered unstable, since it isn't quite the same as what @tt{(}, @tt{[}, and @tt{@"{"} do on contemporary versions of Racket. Those characters' default handlers are sensitive to the values of those parameters at the time the read is @emph{originally started}, not the time they are encountered during the read. For instance, in contemporary versions of Racket, if @racket[(read-accept-dot)] is @racket[#t] at the time @racket[read] is first called and then a custom reader syntax causes it to be set to @racket[#f], a subsequent occurrence of @tt{(} in the same read will be processed as though @racket[(read-accept-dot)] were still @racket[#t].
}

@defproc[
  (parendown-color-lexer
    [weak-open-paren (and/c string? immutable?)]
    [original-get-info (-> any/c any/c any)])
  procedure?
]{
  Given the syntax of a weak opening paren as a string (e.g., @racket["#/"] or @racket["/"]), and given a language's @racket[_get-info] procedure (like one returned by @racket[read-language]), returns a procedure that a @racket[_get-info] procedure can return in response to a request for @racket['color-lexer]. This lexer implements syntax highlighting in nearly the same way @racket[original-get-info] does, but it recognizes @racket[weak-open-paren] as a parenthesis.
}
