CL-WHO - Forked version of Edi Weitz's HTML generator with support for
         macro definition, XML namespaces and more

What follows is the description of some additions in this fork of
CL-WHO, which are in short:  Support for XML namespaces, setting of
output character case, inline macros and precompiled binary output.
More documentation is also available in doc/index.html.

## XML namespaces

Basically, you can now use the syntax `(PREFIX . TAG)` in every position
normally only a keyword would work.  The special `PREFIX` values `T` and
`NIL` designate using the `*XML-NAMESPACE*` variable as prefix and no
prefix at all:

    (with-html-output (*standard-output* NIL)
      ((:xsl . stylesheet) :version 1.0
        ((:xsl . template) :match "@*|node()"
          ((:xsl . copy)
            ((:xsl . apply-templates) :select "@*|node()")))))

    (with-html-output (*standard-output* NIL)
      ((:p :class "foo") "Paragraph")
      ((("html" . p) (:html . "class") "foo") "Paragraph"))

Since a improper one-element list or a cons with a non-`NIL` `CDR` is now
always a tag/attribute, the following examples will run successfully and
generate the tag or attribute `"esc:foo"` respectively:

    (with-html-output (*standard-output* NIL)
      ((esc . "foo") "Paragraph"))

    (with-html-output (*standard-output* NIL)
      ((:p (esc . "foo") "blub") "Paragraph"))

Because changing the namespace might happen to a whole block of XML, the
`XMLNS` macro and the `*XML-NAMESPACE*` and `*XML-ATTRIBUTE-NAMESPACE*`
variables are added to make life easier.  The argument to `XMLNS` may be
a single value, or a two-argument list where the first specifies the tag
and the second the attribute namespace:

    (with-html-output-to-string (*standard-output* NIL)
      (xmlns NIL
        (:html :style "foo"
         (xmlns :xsl
           (:stylesheet :bar "foo")))))

is the same as:

    (with-html-output-to-string (*standard-output* NIL)
      (xmlns (NIL NIL)
        (:html :style "foo"
         (xmlns (:xsl NIL)
           (:stylesheet :bar "foo")))))

The macro always sets both namespaces at the same time.  Manually
specified namespaces in the before mentioned syntax are not influenced
by this (except when the prefix is `T` of course).  (If you want to set
only one value at a time, use the XMLNS* macro, which accepts keyword
arguments `:TAG` and `:ATTR` respectively.)

## Token case

To generalize the `*DOWNCASE-TOKENS-P*` flag, it was exchanged with
`*TOKEN-CASE*`, with the usual values `:UPCASE`, `:DOWNCASE` (default),
`:CAPITALIZE` and `NIL` for no case change.  The internal macro
`TOKEN-CASE` helps to change this inside of a template:

    (with-html-output-to-string (*standard-output* NIL)
      (:html
       (token-case :capitalize
         (:stylesheet :foo "foo")))))

I currently don't know if it's useful to split this into tag, attribute
and prefix variants, so I'll leave it be for now.

## Inline macros

Actually the above mentioned internal macros are executed using another
facility, which doesn't break the stream into multiple chunks of
`WRITE-STRING`.  It doesn't use macros but functions, which are recorded
in `*WHO-INTERNAL-MACROS*` and executed inline with the
`TREE-TO-TEMPLATE` function.  It isn't refactored and polished yet, but
works good enough.

## Binary output

The `WITH-HTML-OUTPUT-TO-STRING-BINARY` macro may be used to convert all
literal strings into byte vectors according to a given external format.
FLEXI-STREAMS is used to both converted literals and support output via
a `FLEXI-STREAM` for dynamic content.

    (with-html-output-binary (stream *standard-output*)
      (:body
       (:p "Hello, World!"
           (str "foo"))))

Which works like the regular macro, except that now the `*COMPILEP*`
variable (or the `COMPILEP` keyword) controls whether the output is
converted to byte vectors.

    (macroexpand
     '(with-html-output-binary (stream *standard-output* binary :compilep T)
        (:body
          (:p "Hello, World!"
              (str "foo")))))
    
    =>
    
    (LET* ((BINARY *STANDARD-OUTPUT*)
           (STREAM
            (FLEXI-STREAMS:MAKE-FLEXI-STREAM BINARY :EXTERNAL-FORMAT
                                             '(:UTF-8 :EOL-STYLE :LF))))
      (PROGN
       (WRITE-SEQUENCE
        #(60 98 111 100 121 62 60 112 62 72 101 108 108 111 44 32 87 111 114 108
          100 33)
        BINARY)
       (LET ((#:G1465 "foo"))
         (WHEN #:G1465 (PRINC #:G1465 STREAM)))
       (WRITE-SEQUENCE #(60 47 112 62 60 47 98 111 100 121 62) BINARY)))
