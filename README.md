# Parendown

```
(a b #/c d) becomes (a b (c d))
```

One syntactic sugar leads to so many quality-of-life improvements in a language.

It's as though we get simple compositions of macros and functions for free because we can write them directly, without any extraneous indentation or parentheses.

```
(foo
  (bar a b c
    d
    e))

can now be written

(foo #/bar a b c
  d
  e)
```

We can add and remove early-exit conditionals and continuation-passing style commands without changing the indentation level of subsequent lines:

```
(foo a b
  c
  (bar d e
    f
    (baz g h
      i
      j)))

can now be written

(foo a b
  c
#/bar d e
  f
#/baz g h
  i
  j)
```

We can add or remove elements from the middle of a linked-list-like data structure without needing to adjust the number of parens at the end, and without a tailor-made macro or variadic function for the purpose:

```
(cons a (cons b (cons c null)))

can now be written

(cons a #/cons b #/cons c null)
```


## Usage

Parendown is a language extension for Racket. To use it, `raco pkg install parendown`, and then if you usually write something like `#lang racket` at the top of your files, write something like `#lang parendown racket` instead. Since Parendown is sugar for parentheses, it'll come in handy for just about any s-expression-based language.

If you're writing your own reader extensions, you can add Parendown functionality to your readtable like so:

```
(require (only-in parendown parendown-readtable-handler))

(make-readtable (current-readtable) #\/ 'dispatch-macro
  parendown-readtable-handler)
```

This gives you the opportunity to use a syntax other than `#/` if you prefer.

In certain circumstances, such as Scribble documentation examples, a new reader syntax is not convenient. In this case, you can achieve a similar effect by using Parendown as a library, importing the `pd` macro:

```
(require (only-in parendown pd))

(pd / foo a b
  c
/ bar d e
  f
/ baz g h
  i
  j)
```
```

The `pd` form expects its first subform to be a symbol -- here `/` -- and then it traverses deeply over the remaining subforms' s-expression structure processing all occurrences of the same symbol. This means you often only have to use the `pd` macro once per module:

```
(pd / begin

  ; ... write module here ...

)
```

The `pd` form also expands calls of the form `(pd (a b c))` simply to `(a b c)`, just so that if you're making nested calls to `pd` with the same symbol, the program continues to work.


## Related work

These syntaxes take primary inspiration from the Arc language's abbreviation of `(a (b c))` as `(a:b c)`. Arc restricted this to a single symbol, but I think I've heard of similar generalizations of this syntax before I developed mine, particularly appearing in alternative implementations of Arc such as suzuki's Semi-Arc and early versions of dido's Arcueid. I've also heard of this in some versions of Pauan's Nulan.

The Haskell operator `$` predates all of these, and it has the very similar effect of allowing `(a b $ c d)` instead of `(a b (c d))` for function calls. In fact, the benefits of this sugar in continuation-passing style were known at least as far back as the Haskell 1.2 report from 1992 (page 85):

```
-- right-associating infix application operator (useful in continuation-
-- passing style)
($)                     :: (a -> b) -> a -> b
f $ x                   =  f x
```

Once I implemented the sugar `(a b /c d)` for my own new Lispy languages, I started to change my indentation style, which finally let me avoid indentation for continuation-passing style code. In the design of the Cene language, I had this sugar in place from the start, so monadic and continuation-passing style techniques were so easy to use that I didn't even feel a great need for side effects.
