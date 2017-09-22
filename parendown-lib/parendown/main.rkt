#lang racket/base

(require
  (only-in racket/undefined undefined)
  (only-in syntax/readerr raise-read-error))

(provide parendown-readtable-handler)


(define (until-fn condition body)
  (unless (condition)
    (body)
    (until-fn condition body)))

(define-syntax-rule (until condition body ...)
  (until-fn (lambda () condition) (lambda () body ...)))

; Racket's `peek-char` lets you skip a number of *bytes*, but not a
; number of characters. This one lets you skip a number of characters.
;
; TODO: There's gotta be a more efficient way to do this, right?
;
(define (peek-char-skipping-chars in skip-chars-amt)
  (let loop ([bytes-amt-to-attempt 0])
    (define peeked-byte (peek-byte in bytes-amt-to-attempt))
    (if (eof-object? peeked-byte)
      peeked-byte
      (let ([peeked-string (peek-string bytes-amt-to-attempt 0 in)])
        (if (< skip-chars-amt (string-length peeked-string))
          (string-ref peeked-string skip-chars-amt)
          (loop (add1 bytes-amt-to-attempt)))))))

(define (non-terminating-char? readtable x)
  (and (char? x)
    (let ()
      (define-values
        (char-terminating char-entry char-dispatch-entry)
        (readtable-mapping readtable x))
      (or (eq? 'non-terminating-macro char-terminating)
        (and (char? char-terminating)
          (parameterize ([current-readtable #f])
            (define symbol-name (string #\a char-terminating))
            (not
              (eq? 'a (read (open-input-string symbol-name))))))))))


(define parendown-readtable-handler
  (case-lambda
    [ (name in)
      (define-values (line col pos) (port-next-location in))
      (define src (object-name in))
      (read-list name in src line col pos #f)]
    [ (name in src line col pos)
      (read-list name in src line col pos #t)]))

(define (read-list name in src line col pos should-read-syntax)
  (define span 1)
  (define (read-as-we-should [in in])
    (if should-read-syntax
      (read-syntax/recursive src in)
      (read/recursive in)))
  (define (read-skipping-comments)
    (define result (read-as-we-should))
    (until (not (special-comment? result))
      (set! result (read-as-we-should)))
    result)
  (define (skip-whitespace)
    (regexp-match #px"^\\s*" in))
  (define (like-default char . originals)
    (define-values (char-terminating char-entry char-dispatch-entry)
      (readtable-mapping (current-readtable) char))
    (memq char-terminating originals))
  
  ; NOTE: Racket doesn't provide any really elegant way to skip
  ; comments without reading the next non-comment expression... but it
  ; turns out the built-in list syntax doesn't *use* any really
  ; elegant way to do it either. So, the function
  ; `peek-after-whitespace-and-comments` here is full of ad hoc,
  ; unhygienic behavior, but almost all of it is meant to simulate
  ; specific behaviors of the built-in list syntax.
  (define (peek-after-whitespace-and-comments)
    (skip-whitespace)
    (define next-char (peek-char in))
    
    (cond
      [(eof-object? next-char) next-char]
      [ (like-default next-char #\;)
        (read-as-we-should)
        (peek-after-whitespace-and-comments)]
      [(not (like-default next-char #\#)) next-char]
      [#t
        (define-values (dispatch-line dispatch-col dispatch-pos)
          (port-next-location in))
        (define dispatch-span 2)
        (define hash-char next-char)
        (define fake-dispatch-string (peek-string 2 0 in))
        (cond
          [(< (string-length fake-dispatch-string) 2) hash-char]
          [ (like-default (string-ref fake-dispatch-string 1) #\;)
            (read-string 2 in)
            ; NOTE: Inside of the built-in list syntax, a #;(...)
            ; comment works like this even if ; has been bound to a
            ; custom `dispatch-macro` in the current readtable. Inside
            ; the expression, the readtable seems to be the same as
            ; it is on the outside, so the built-in list syntax does
            ; *not* parameterize the readtable with a default
            ; implementation of #; to do the read here.
            (read-skipping-comments)
            (peek-after-whitespace-and-comments)]
          [ (like-default (string-ref fake-dispatch-string 1) #\|)
            (read-string 2 in)
            
            ; NOTE: Inside of the built-in list syntax, a #|...|#
            ; comment isn't picky about which exact characters are
            ; used to close it, as long as they have the appropriate
            ; `readtable-mapping`. Outside of a list, a #|...|#
            ; comment is pickier than that.
            (define (read-rest-of-fake-nested-comment)
              (define first (read-char in))
              (define second (peek-char in))
              (when (or (eof-object? first) (eof-object? second))
                (raise-read-error "read: end of file in #| comment"
                  src
                  dispatch-line dispatch-col dispatch-pos
                  dispatch-span))
              
              (cond
                [ (like-default first #\#)
                  (if (like-default second #\|)
                    (begin
                      (read-char in)
                      (read-rest-of-fake-nested-comment)
                      (read-rest-of-fake-nested-comment))
                    (read-rest-of-fake-nested-comment))]
                [ (like-default first #\|)
                  (if (like-default second #\#)
                    (read-char in)
                    (read-rest-of-fake-nested-comment))]
                [#t (read-rest-of-fake-nested-comment)]))
            
            (read-rest-of-fake-nested-comment)
            (peek-after-whitespace-and-comments)]
          [#t hash-char])]))
  
  (define (closing? char)
    (like-default char #\) #\] #\}))
  
  ; TODO: The built-in list syntax actually seems to obtain the value
  ; of these parameters at the time the *original* read procedure was
  ; called, before any recursive reads. For instance, the expression
  ; (1 2 * (3 . 4) 5), where * is a custom reader macro that sets
  ; `read-accept-dot` to #f, reads as (1 2 (3 . 4) 5) if
  ; `read-accept-dot` was true before the read. See if there's a way
  ; we can simulate that.
  (define accept-dot (read-accept-dot))
  (define accept-infix-dot (read-accept-infix-dot))
  
  ; These variables are the state of the next loop.
  (define rev-elems (list))
  (define improper-tail (list))
  (define action-on-non-comment void)
  (define next-char undefined)
  (define listening-for-dots accept-dot)
  (until
    (begin
      (skip-whitespace)
      (set! next-char (peek-after-whitespace-and-comments))
      
      (when (eof-object? next-char)
        (raise-read-error
          (string-append
            "read: expected `)', `]', or `}' to close "
            "`" (string name) "'")
          src line col pos span))
      
      (if
        (and listening-for-dots
          (like-default next-char #\.)
          (not
            (non-terminating-char? (current-readtable)
              ; TODO: This is the only place we peek more than one
              ; character ahead. See if the official reader performs a
              ; read and then a peek here instead.
              (peek-char-skipping-chars in 1))))
        (let ()
          (define-values (dot-line dot-col dot-pos)
            (port-next-location in))
          (define dot-span 1)
          (define dot-char next-char)
          (define (dot-err [dot-char dot-char])
            (raise-read-error
              (string-append
                "read: illegal use of `" (string dot-char) "'")
              src dot-line dot-col dot-pos dot-span))
          (define (peek-after-whitespace-and-comments-force)
            (define next-char (peek-after-whitespace-and-comments))
            (when (like-default next-char #\#)
              (read-char in)
              (dot-err))
            next-char)
          
          (read-char in)
          (define elem (read-skipping-comments))
          (define possible-next-dot-or-closing
            (peek-after-whitespace-and-comments-force))
          (cond
            [(eof-object? possible-next-dot-or-closing) (dot-err)]
            [ (and accept-infix-dot
                (like-default possible-next-dot-or-closing #\.))
              
              (read-char in)
              
              ; NOTE: In the built-in list syntax, the syntax
              ; (1 2 . 3 . *), where * is a custom reader macro that
              ; returns a special comment, reads as (1 2). For parity,
              ; we don't add the infix operator to the list until we
              ; encounter the next non-comment list element. Note that
              ; comments like ; #; and #| are skipped a different way
              ; (emulated here by
              ; `peek-after-whitespace-and-comments`), so they don't
              ; exhibit the same behavior.
              (set! action-on-non-comment
                (lambda ()
                  (set! action-on-non-comment void)
                  (set! rev-elems (append rev-elems (list elem)))))
              
              (set! listening-for-dots #f)
              (define possible-closing
                (peek-after-whitespace-and-comments-force))
              (when (eof-object? possible-closing)
                ; TODO: This prints garbage to the console, but this
                ; has parity with Racket. See if this is a bug that
                ; needs to be fixed in Racket.
                (dot-err (integer->char #xFFFD)))
              (when (closing? possible-closing)
                (dot-err possible-closing))
              
              ; We've read the operator of an infix list, and there's
              ; still more to go after that, so we continue the loop.
              #f]
            [ (closing? possible-next-dot-or-closing)
              (set! improper-tail elem)
              (set! next-char possible-next-dot-or-closing)
              
              ; We've read the end of an improper list, so we exit the
              ; loop.
              #t]
            [ #t
              
              ; The usual error for (10 20 . 30 40) reads the "4" from
              ; the stream before it raises its error, so we do the
              ; same thing.
              (read-char in)
              
              (dot-err)]))
        
        ; If the next character is not a dot, or if we're not
        ; listening for dots, we exit the loop if we've reached a
        ; closing paren.
        (closing? next-char)))
    (define elem (read-as-we-should))
    (unless (special-comment? elem)
      (action-on-non-comment)
      (set! rev-elems (cons elem rev-elems))))
  (define result (append (reverse rev-elems) improper-tail))
  (define-values (stop-line stop-col stop-pos)
    (port-next-location in))
  (when should-read-syntax
    (set! result
      (datum->syntax #f result
        (and line (vector src line col pos (- stop-pos pos)))))
    (when (like-default next-char #\])
      (set! result (syntax-property result 'paren-shape #\[)))
    (when (like-default next-char #\})
      (set! result (syntax-property result 'paren-shape #\{))))
  result)
