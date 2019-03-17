# Parendown

[![Travis build](https://travis-ci.org/lathe/parendown-for-racket.svg?branch=master)](https://travis-ci.org/lathe/parendown-for-racket)

Parendown adds *weak opening parentheses* to Racket. It's a syntax sugar, and it's as simple as this:

```
(a b #/c d) becomes (a b (c d))
```

It has some pretty straightforward uses in terms of saving closing parentheses. When we write `(not #/equal? a b)`, it's as though we had an operation called "`not #/equal?`" already; we wouldn't get much benefit from defining `unequal?` except to shave a few characters off the name. If Racket didn't supply `(last x)`, we could still write `(car #/reverse x)` a few times before we got around to defining it ourselves.

Those simple use cases were possible in Arc using its `a:b` syntax; they could be written as `(no:iso a b)` and `(car:rev x)` respectively. Parendown was inspired by experience using Arc, but its syntax is generalized to allow more (or less) than one list element before the `#/`. This generalization leads to several benefits.

In Racket, Parendown is a language extension which adds a `#/` reader syntax. You can use it with `#lang parendown <another language>`, like so:

```
#lang parendown racket/base

(displayln #/string-append "Hello, " "world!")
```

The `#/` reader syntax is designed to behave as much like the standard opening paren `(` as possible, but doesn't consume the closing paren `)`. It leaves that paren in the stream. Since only a strong opening paren `(` will consume a closing paren, this means a single closing paren `)` will tend to match up with zero or more weak opening parens `#/` *on its way* to matching up with the opening paren `(`.

Although it doesn't usually make a difference in Racket code, the weak opening paren `#/` will also match up with square and curly closing brackets as though it were an occurrence of the appropriate square or curly opening bracket:

```
[a b #/c d] becomes [a b [c d]]
{a b #/c d} becomes {a b {c d}}
```

As simple as Parendown is as a syntax sugar, its influence casts ripples over a whole language design. It singlehandedly leads to several different quality-of-life improvements throughout the use of the Racket language. It solves several things at once that have usually been solved with multiple specialized sugars, DSLs, or even runtime features.


## Parendown and higher-order functions vs. macros

For instance, a common pattern in Lisp-based languages is that for every higher-order function, there tends to be a macro to make that function easier to use. Racket has some macros (aka syntax transformers) like `for/list` for iterating over sequences:

```
(for/list ([a (in-list (list 1 2 3))]
           [b (in-list (list 10 20 30))])
  (* a b))
```

Using Parendown, and using some `list-bind` and `list-map` operations from [Lathe Comforts](https://github.com/lathe/lathe-comforts-for-racket) (a library which is designed for use with Parendown), the way to write this using higher-order functions becomes just about as concise as the macro version:

```
(list-bind (list 1 2 3) #/lambda (a)
#/list-map (list 10 20 30) #/lambda (b)
  (* a b))
```

Since this code takes up the same number of lines and the same amount of indentation, it usually has the same impact on the large-scale brevity of the codebase.

The `for/list` syntax is an example of something languages like Haskell use monadic style to achieve, including the monadic `do` DSL:

```
-- Haskell, using monadic `do` notation:
do a <- [1, 2, 3]
   b <- [10, 20, 30]
   return (a * b)

-- A more manual monadic style in Haskell using lambdas (\var -> body)
[1, 2, 3] >>= \a ->
[10, 20, 30] >>= \b ->
return (a * b)
```

Monadic style in general acts as a way to build continuation-passing style programs. A value of a monadic type (at least in a higher-order language) is something which can have continuations passed to it; the property of being monadic tells us little more than that a type supports a well-behaved operation (called "bind" or `>>=`) for taking a value of that type and passing a continuation to it. (Lists are like very simple computations which consist of many possible "results," which is why they're a well-behaved monadic type.)

Using Parendown, we've already seen how to write code that's roughly in parity with Haskell's monadic style. We've only seen this technique applied to list construction, but it does come in handy in other places we use continuation-passing style as well. However, those situations actually don't come up all that much in Racket, since Racket has first-class continuations, so we'll stick to the list example.

Continuation-passing style can be annoying to deal with for several reasons; it causes code to become very nested, full of intermediate variables, and sequentialized. Parendown helps specifically with the nesting. If we write the `list-bind` and `list-map` without Parendown, we can see a pyramid forming that pushes our code to the right as we go along:

```
(list-bind (list 1 2 3)
  (lambda (a)
    (list-map (list 10 20 30)
      (lambda (b)
        (* a b)))))
```

In a more traditional Lispy style, the pyramid might be even more voluminous and unruly. Here we use a common Lisp indentation style and use Racket's own `append-map` and `map` operations rather than using Lathe Comforts:

```
(append-map (lambda (a)
              (map (lambda (b)
                     (* a b))
                   (list 10 20 30)))
            (list 1 2 3))
```

The sparse style isn't all bad. It gives the code some distinct visual landmarks, and the generous indentation makes it easy to spot all the arguments to `append-map` at a glance. But the arguments in the above `list-bind` example are easy to spot at a glance thanks to a different feature: That all but one of them fits on a single line. In more complex situations, where the layers of nesting are deep, the `list-bind` style -- especially with Parendown in there to flatten the indentation -- remains just as readable, whereas the `append-map` call ends up having its second argument stranded on a later screenful of code, out of sight, making it no longer easy to associate it with `append-map` at a single glance.

The `for/list` macro isn't the only example of a Racket sugar that's made somewhat redundant by the Parendown sugar. Here are a few more in the same vein as `for/list`, where the Parendown sugar makes it easy to pass in a callback or write multiple layers of functionality without introducing an extra layer of indentation:

```
; Without Parendown:
(let/cc k
  ...)

; Without `let/cc`, but with Parendown:
(call/cc #/lambda (k)
  ...)


; Without Parendown:
(let* ([a ...]
       [b ...])
  ...)

; Without `let*`, but with Parendown:
(let ([a ...])
#/let ([b ...])
  ...)


; Without Parendown:
(cond
  [(list? x) (length x)]
  [(integer? x) x]
  [else 0])

; Without `cond`, but with Parendown:
(if (list? x) (length x)
#/if (integer? x) x
  0)
```

The `let` and `if` synergies are especially nice. Code that uses Parendown can very easily set up intermediate variables with `let` or early exit conditions with `if` without introducing a single indentation level. These are conveniences which a Racket programmer might otherwise consider achieving using locally scoped `(define ...)` forms or escape continuations, but Parendown makes it unnecessary to bring in those complex techniques.


## Parendown's other uses

Parendown has a few other uses, although these start to be less compelling.

Parendown can occasionally have advantages similar to infix syntax:

```
; In an infix lang, we may refactor like this, making a small edit:
position + width
position + 0.5 * width

; With Parendown, we may refactor like this, making a small edit:
(+ position width)
(+ position #/* 0.5 width)
```

If there's a useful variable-arity operation, Parendown can sometimes help us tinker around with possibilities for it before we realize what its design should be:

```
; Without Parendown:
(* a b c d)

; Without variable-arity `*` but with Parendown:
(* a #/* b #/* c d)


; Without Parendown:
(- a b c d)

; Without variable-arity `-` but with Parendown:
(- a #/+ b c d)


; Without Parendown:
(list a b c)

; If for some reason we didn't have `list` but had Parendown:
(cons a #/cons b #/cons c null)
```

In fact, once we have `list` in the language, it has such synergy with Parendwn that we might neglect to define any other variable-arity functions for a while:

```
; Without Parendown:
(append
  a
  b
  c)

; Without `append`, but with Parendown:
(append* #/list
  a
  b
  c)
```

Lisp syntax is known for having very uniform notation, but an exception is made in almost every Lisp dialect for quotation. With Parendown, quoted lists could use roughly the same amount of code lines and indentation as ever, without the need for a specialized notation:

```
; Without Parendown:
'(/
   (+ (- b) (sqrt (- (expt b 2) (* 4 a c))))
   (* 2 a))

; Without the quotation syntax, but with Parendown:
(quote #/ /
  (+ (- b) (sqrt (- (expt b 2) (* 4 a c))))
  (* 2 a))
```

One of the hallmark syntax sugars/DSLs of Clojure is its suite of *threading macros*, which allow long sequences of functional transformations to be written in a step-by-step way. (Alexis King has written a Clojure-inspired [`threading` package for Racket](https://github.com/lexi-lambda/threading), which we'll use for this example.) One of the possible benefits of this step-by-step juxtaposition is to avoid an indentation pyramid, so what comes naturally in Parendown isn't far off from the Clojure threaded style:

```
; Without Parendown, with Clojure-like `~>>` from package `threading`:
(~>> users
  (append-map user-friends)
  (filter (lambda (user) (not (user-banned? user))))
  (map user-name)
  string->immutable-string
  (foldl
    (lambda (name result) (hash-update result name add1 0))
    (hash)))

; Without `~>>` but with Parendown (writing steps from last to first):
(foldl
  (lambda (name result) (hash-update result name add1 0))
  (hash)
#/string->immutable-string
#/map user-name
#/filter (lambda (user) #/not #/user-banned? user)
#/append-map user-friends
  users)
```

When a flat sequence of steps doesn't emerge on its own, or when we really want the steps to be arranged from first to last like they are in Clojure, it's not hard to approximate that style even without using Parendown:

```
; Without `~>>` but with `let*` (writing steps from first to last):
(let* ([- users]
       [- (append-map user-friends -)]
       [- (filter (lambda (user) (not (user-banned? user))) -)]
       [- (map user-name -)]
       [- (string->immutable-string -)]
       [- (foldl
            (lambda (name result) (hash-update result name add1 0))
            (hash)
            -)])
  -)
```


## Commentary on Parendown and variable shadowing

The last example of how to emulate Clojure threading makes use of variable shadowing; it doesn't rely on Parendown at all. Nevertheless, the two features have some interesting overlaps, rooted in their similarities at a syntactic level: The syntactic pattern "variable binding ... shadowing variable binding ... variable usage site" is similar to the pattern `( ... #/ ... )`.

For both variable shadowing and Parendown, we have a kind of *lexical* state update going on. The stateful entity here is not part of the program's run time operation, but part of the operation of the codebase itself as a maintainable system. A simple and local *edit to the code* can immediately change a valid use of one variable binding into a valid use of a different one (in the case of variable shadowing) or change one well-matched system of parentheses into another (in the case of Parendown's weak opening parens). What makes this in some sense stateful is that there's an entity that has an unchanging identity (the variable name, or the closing paren occurrence), and it has a changing state (the expression or parameter the variable is bound to, or the set of weak opening parens that match up with that closing paren).

In this way, Parendown and variable shadowing are techniques that should be adopted or avoided on the basis of how the code is edited. One parts of the code may undergo edits in such a way where Parendown's ability to approximate infix syntax comes in handy. Another part may involve two nested variable bindings which could easily use the same name, but for which we expect it to be a mistake if a future maintainer switches one for the other, so it's best for their names to be distinct until further notice. Of course, since a programmer can come in and refactor a variable name or substitute a strong opening paren for a weak one at any time, this kind of decision is always reversible.


## Installation and use

This is a library for Racket. To install it from the Racket package index, run `raco pkg install parendown`. Then you can change the `#lang` line of your Racket modules to `#lang parendown <other language>`, where `#lang <other language>` is the line you were using before. Since Parendown is sugar for parentheses, it'll be a handy extension to just about any Racket language where parentheses have their usual Racket behavior.

To install it from source, run `raco pkg install --deps search-auto` from the `parendown-lib/` directory.

[Documentation for Parendown for Racket](http://docs.racket-lang.org/parendown/index.html) is available at the Racket documentation website, and it's maintained in the `parendown-doc/` directory.

If you're writing your own reader extensions, you can add Parendown functionality to your readtable like so:

```
(require (only-in parendown parendown-readtable-handler))

(make-readtable (current-readtable) #\/ 'dispatch-macro
  parendown-readtable-handler)
```

This gives you the opportunity to use a syntax other than `#/` if you prefer.

In certain circumstances, it's inconvenient to change the reader. Most of the advantages of Parendown are also available in the form of the `pd` syntax transformer:

```
#lang racket/base

(require (only-in parendown pd))

(pd / begin
  (displayln / string-append "Hello, " "world!")
  (displayln / string-append "I can't use " "division!"))
```

The `(pd / ...)` form surrounds some code and processes all occurrences of the symbol `/` it encounters. It also lets you switch to something like `(pd % ...)` if you want it to process occurrences of `%` instead, although typically you could just `(define div /)` outside the `pd` form in any case where you need to use division.

You can use `pd` any number of times, but typically it's sufficient to surround a chunk of code with `(pd / begin ...)`.

The `pd` form also expands calls of the form `(pd (a b c))` simply to `(a b c)`. This ensures that if the code contains nested calls like `(pd / - / + 1 (pd / add1 / add1 0))`, everything continues to work.


## Related work

These syntaxes take primary inspiration from the Arc language's abbreviation of `(a (b c))` as `(a:b c)` (which only worked when `a` and `b` were symbols), as well as a `(scope let a 1 @ let b 2 @ + a b)` syntax [posted by Yuval Lando on Arc Forum](http://arclanguage.org/item?id=11934). Ross Angle (rocketnia) developed some languages (including what's become [Era's Cene language](https://github.com/era-platform/cene-for-racket)) which renamed this `:` to `/` and generalized it. Parendown (another project started by that author) brings that generalized syntax to Racket.

At some point, Pauan's Nulan project may have used a syntax like this as well.

The Haskell operator `$` predates all of these, and it has the very similar effect of allowing `(a b $ c d)` instead of `(a b (c d))` for function calls in that language. In fact, the benefits of this sugar in continuation-passing style were known at least as far back as the Haskell 1.2 report from 1992 (page 85):

```
-- right-associating infix application operator (useful in continuation-
-- passing style)
($)                     :: (a -> b) -> a -> b
f $ x                   =  f x
```

Even 28 years earlier than that (1974), [Interlisp](http://bitsavers.trailing-edge.com/pdf/xerox/interlisp/Interlisp_Reference_Manual_1974.pdf) had a similar behavior. It called `[` and `]` "super-parentheses," and the combination of `[`, `(`, and `]` in Interlisp worked roughly like the combination of `(`, `#/`, and `)` does in a `#lang parendown racket` program:

```
The INTERLISP read program treats square brackets as 'super-parentheses': a
right square bracket automatically supplies enough right parentheses to match
back to the last left square bracket (in the expression being read), or if none
has appeared, to match the first left parentheses,
e.g.,    (A (B (C]=(A (B (C))),
         (A [B (C (D] E)=(A (B (C (D))) E).
```

[A 2006 paper by Anssi Yli-Jyrä](http://www.linguistics.fi/julkaisut/SKY2006_1/2.6.9.%20YLI-JYRA.pdf) reviews a few different designs, including the Interlisp design. That author ultimately favors the following approach, where this time `[`, `〈`, and `]` serve the same purposes as `(`, `#/`, and `)` serve with Parendown:

> Krauwer and des Tombe (1981) proposed _condensed labelled bracketing_ that can be defined as follows. Special brackets (here we use angle brackets) mark those initial and final branches that allow an omission of a bracket on one side in their realized markup. The omission is possible on the side where a normal bracket (square bracket) indicates, as a side-effect, the boundary of the phrase covered by the branch. For example, bracketing "[[A B] [C [D]]]" can be replaced with "[A B〉 〈C 〈D]" using this approach.
