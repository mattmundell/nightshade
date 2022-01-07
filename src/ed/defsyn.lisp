;;; This file contains definitions of various character attributes.

(in-package "ED")

#[ System Defined Character Attributes

These are predefined:

  `Whitespace'
     A value of 1 indicates the character is whitespace.

  `Word Delimiter'
     A value of 1 indicates the character separates words (see section
     [Text Functions]).

  `Digit'
     A value of 1 indicates the character is a base ten digit.  This may be
     shadowed in modes or buffers to mean something else.

  `Space'
     This is like `Whitespace', but it should not include Newline.
     The editor uses this primarily for handling indentation on a line.

  `Sentence Terminator'
     A value of 1 indicates these characters terminate sentences (see section
     reftext-functions).

  `Sentence Closing Char'
     A value of 1 indicates these delimiting characters, such as "
     or ), may follow a `Sentence Terminator' (see section
     reftext-functions).

  `Paragraph Delimiter'
     A value of 1 indicates these characters delimit paragraphs when they begin
     a line (see section reftext-functions).

  `Page Delimiter'
     A value of 1 indicates this character separates logical pages (see section
     reflogical-pages) when it begins a line.

  `Scribe Syntax'
     This uses the following symbol values:

          nil
             These characters have no interesting properties.

          :escape
             This is @@ for the Scribe formatting language.

          :open-paren
             These characters begin delimited text.

          :close-paren
             These characters end delimited text.

          :space
             These characters can terminate the name of a formatting command.

          :newline
             These characters can terminate the name of a formatting command.


  `Lisp Syntax'
     This uses symbol values from the following:

          nil
             These characters have no interesting properties.

          :space
             These characters act like whitespace and should not include
             Newline.

          :newline
             This is the Newline character.

          :open-paren
             This is ( character.

          :close-paren
             This is ) character.

          :prefix
             This is a character that is a part of any form it precedes --
             for example, the single quote, '.

          :string-quote
             This is the character that quotes a string literal,
             ".

          :char-quote
             This is the character that escapes a single character,
             \.

          :comment
             This is the character that makes a comment with the rest of the
             line, ;.

          :constituent
             These characters are constitute symbol names.
]#

(defattribute "Whitespace"
  "A value of 1 for this attribute indicates that the corresponding character
  should be considered as whitespace.  This is used by the Blank-Line-P
  function.")

(setf (character-attribute :whitespace #\space) 1)
(setf (character-attribute :whitespace #\linefeed) 1)
(setf (character-attribute :whitespace #\tab) 1)
(setf (character-attribute :whitespace #\newline) 1)

(defattribute "Word Delimiter"
  "A value of 1 for this attribute indicates that the corresponding character
  separates words.  This is used by the word manipulating commands.")

(setf (character-attribute :word-delimiter nil) 1)
(setf (character-attribute :word-delimiter #\!) 1)
(setf (character-attribute :word-delimiter #\@) 1)
(setf (character-attribute :word-delimiter #\#) 1)
(setf (character-attribute :word-delimiter #\$) 1)
(setf (character-attribute :word-delimiter #\%) 1)
(setf (character-attribute :word-delimiter #\^) 1)
(setf (character-attribute :word-delimiter #\&) 1)
(setf (character-attribute :word-delimiter #\*) 1)
(setf (character-attribute :word-delimiter #\() 1)
(setf (character-attribute :word-delimiter #\)) 1)
(setf (character-attribute :word-delimiter #\-) 1)
(setf (character-attribute :word-delimiter #\_) 1)
(setf (character-attribute :word-delimiter #\=) 1)
(setf (character-attribute :word-delimiter #\+) 1)
(setf (character-attribute :word-delimiter #\[) 1)
(setf (character-attribute :word-delimiter #\]) 1)
(setf (character-attribute :word-delimiter #\\) 1)
(setf (character-attribute :word-delimiter #\|) 1)
(setf (character-attribute :word-delimiter #\;) 1)
(setf (character-attribute :word-delimiter #\:) 1)
(setf (character-attribute :word-delimiter #\') 1)
(setf (character-attribute :word-delimiter #\") 1)
(setf (character-attribute :word-delimiter #\{) 1)
(setf (character-attribute :word-delimiter #\}) 1)
(setf (character-attribute :word-delimiter #\,) 1)
(setf (character-attribute :word-delimiter #\.) 1)
(setf (character-attribute :word-delimiter #\<) 1)
(setf (character-attribute :word-delimiter #\>) 1)
(setf (character-attribute :word-delimiter #\/) 1)
(setf (character-attribute :word-delimiter #\?) 1)
(setf (character-attribute :word-delimiter #\`) 1)
(setf (character-attribute :word-delimiter #\~) 1)
(setf (character-attribute :word-delimiter #\space) 1)
(setf (character-attribute :word-delimiter #\linefeed) 1)
(setf (character-attribute :word-delimiter #\formfeed) 1)
(setf (character-attribute :word-delimiter #\tab) 1)
(setf (character-attribute :word-delimiter #\newline) 1)

(shadow-attribute :word-delimiter #\. 0 "Fundamental")
(shadow-attribute :word-delimiter #\' 0 "Text")
(shadow-attribute :word-delimiter #\backspace 0 "Text")
(shadow-attribute :word-delimiter #\_ 0 "Text")

(defattribute "Page Delimiter"
  "This attribute is 1 for characters that separate pages, 0 otherwise.")
(setf (character-attribute :page-delimiter nil) 1)
(setf (character-attribute :page-delimiter #\page) 1)


(defattribute "Lisp Syntax"
  "These character attribute is used by the lisp mode commands, and possibly
  other people.  The value of ths attribute is always a symbol.  Currently
  defined values are:
   NIL - No interesting properties.
   :space - Acts like whitespace, should not include newline.
   :newline - Newline, man.
   :open-paren - An opening bracket.
   :close-paren - A closing bracket.
   :prefix - A character that is a part of any form it appears before.
   :string-quote - The character that quotes a string.
   :char-quote - The character that escapes a single character.
   :comment - The character that comments out to end of line.
   :constituent - Things that make up symbols."
  'symbol nil)

(setf (character-attribute :lisp-syntax #\space) :space)
(setf (character-attribute :lisp-syntax #\tab) :space)

(setf (character-attribute :lisp-syntax #\() :open-paren)
(setf (character-attribute :lisp-syntax #\)) :close-paren)
(setf (character-attribute :lisp-syntax #\') :prefix)
(setf (character-attribute :lisp-syntax #\`) :prefix)
(setf (character-attribute :lisp-syntax #\#) :prefix)
(setf (character-attribute :lisp-syntax #\,) :prefix)
(setf (character-attribute :lisp-syntax #\") :string-quote)
(setf (character-attribute :lisp-syntax #\\) :char-quote)
(setf (character-attribute :lisp-syntax #\;) :comment)
(setf (character-attribute :lisp-syntax #\newline) :newline)
(setf (character-attribute :lisp-syntax nil) :newline)

(do-alpha-chars (ch :both)
  (setf (character-attribute :lisp-syntax ch) :constituent))

(setf (character-attribute :lisp-syntax #\0) :constituent)
(setf (character-attribute :lisp-syntax #\1) :constituent)
(setf (character-attribute :lisp-syntax #\2) :constituent)
(setf (character-attribute :lisp-syntax #\3) :constituent)
(setf (character-attribute :lisp-syntax #\4) :constituent)
(setf (character-attribute :lisp-syntax #\5) :constituent)
(setf (character-attribute :lisp-syntax #\6) :constituent)
(setf (character-attribute :lisp-syntax #\7) :constituent)
(setf (character-attribute :lisp-syntax #\8) :constituent)
(setf (character-attribute :lisp-syntax #\9) :constituent)

(setf (character-attribute :lisp-syntax #\!) :constituent)
(setf (character-attribute :lisp-syntax #\{) :constituent)
(setf (character-attribute :lisp-syntax #\}) :constituent)
(setf (character-attribute :lisp-syntax #\[) :constituent)
(setf (character-attribute :lisp-syntax #\]) :constituent)
(setf (character-attribute :lisp-syntax #\/) :constituent)
(setf (character-attribute :lisp-syntax #\@) :constituent)
(setf (character-attribute :lisp-syntax #\-) :constituent)
(setf (character-attribute :lisp-syntax #\_) :constituent)
(setf (character-attribute :lisp-syntax #\+) :constituent)
(setf (character-attribute :lisp-syntax #\%) :constituent)
(setf (character-attribute :lisp-syntax #\*) :constituent)
(setf (character-attribute :lisp-syntax #\$) :constituent)
(setf (character-attribute :lisp-syntax #\^) :constituent)
(setf (character-attribute :lisp-syntax #\&) :constituent)
(setf (character-attribute :lisp-syntax #\~) :constituent)
(setf (character-attribute :lisp-syntax #\=) :constituent)
(setf (character-attribute :lisp-syntax #\<) :constituent)
(setf (character-attribute :lisp-syntax #\>) :constituent)
(setf (character-attribute :lisp-syntax #\?) :constituent)
(setf (character-attribute :lisp-syntax #\.) :constituent)
(setf (character-attribute :lisp-syntax #\:) :constituent)


(defattribute "Sentence Terminator"
  "Used for terminating sentences -- ., !, ?.
   Possibly could make type (mod 3) and use the value of 2 and 1 for spaces
   to place after chacter."
  '(mod 2)
  0)

(setf (character-attribute :sentence-terminator #\.) 1)
(setf (character-attribute :sentence-terminator #\!) 1)
(setf (character-attribute :sentence-terminator #\?) 1)
