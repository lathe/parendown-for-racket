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

becomes

(foo#/bar a b c
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

becomes

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
(cons a (cons b (cons c nil)))
becomes
(cons a #/cons b #/cons c nil)
```

## Usage

Parendown is a language extension for Racket. To use it, `raco pkg install parendown`, and then if you usually write something like `#lang racket` at the top of your files, write something like `#lang parendown racket` instead. Since Parendown is sugar for parentheses, it'll come in handy for just about any s-expression-based language.

## Anecdotes

Parendown takes primary inspiration from the Arc language's abbreviation of `(a (b c))` as `(a:b c)`. While Arc restricted this to a single symbol, I think I've heard of similar generalizations of this syntax before I developed mine, particularly appearing in alternative implementations of Arc such as suzuki's Semi-Arc and early versions of dido's Arcueid.

Once I implemented the sugar `(a b /c d)` for my own languages and struck upon the way it let me avoid indentation for continuation-passing style code, I decided to design a bunch of the Cene language with it in place from the start. It has a surprising impact on a language.
