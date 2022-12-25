(defpackage llace/tests/functional-parsing
  (:use :cl :serapeum :parachute :llace/functional-parsing)
  (:shadowing-import-from :parachute :true))
(in-package :llace/tests/functional-parsing)

(define-test @item
  (is equal '() (parse (@item) ""))
  (is equal '((#\s . "up")) (parse (@item) "sup")))

(define-test >>=
  (let ((double-char (>>= (@item) (op (@char _)))))
    (is equal '() (parse double-char ""))
    (is equal '() (parse double-char "a"))
    (is equal '() (parse double-char "ab"))
    (is equal '((#\a . "")) (parse double-char "aa"))
    (is equal '((#\a . "bb")) (parse double-char "aabb"))
    (finish (parse (>>= (@item) (loop)) ""))))

(define-test >>
  (let ((digit-item (>> (@digit) (@item))))
    (is equal '() (parse digit-item ""))
    (is equal '() (parse digit-item "4"))
    (is equal '() (parse digit-item "b"))
    (is equal '() (parse digit-item "b4"))
    (is equal '((#\a . "")) (parse digit-item "4a"))
    (is equal '((#\b . "")) (parse digit-item "4b"))
    (is equal '((#\a . "bc")) (parse digit-item "4abc"))
    (finish (parse (>> (@item) (loop)) ""))))

(define-test @return
  (is equal '((42 . "")) (parse (@return 42) ""))
  (is equal '((42 . "foo")) (parse (@return 42) "foo")))

(define-test @nothing
  (is equal '() (parse (@nothing) ""))
  (is equal '() (parse (@nothing) "foo")))

(define-test either
  (let ((digit-or-item (either (@digit) (@item))))
    (is equal '() (parse digit-or-item ""))
    (is equal '((#\4 . "")) (parse digit-or-item "4"))
    (is equal '((#\b . "")) (parse digit-or-item "b"))
    (is equal '((#\b . "4")) (parse digit-or-item "b4"))
    (is equal '((#\4 . "a")) (parse digit-or-item "4a"))
    (is equal '((#\4 . "abc")) (parse digit-or-item "4abc"))
    (finish (parse (either (@digit) (loop)) "1"))
    (finish (parse (either (@digit) (@letter) (loop)) "a"))))

(define-test @zero-or-more
  (let ((maybe-some-digits (@zero-or-more (@digit))))
    (is equal '((() . "")) (parse maybe-some-digits ""))
    (is equal '(((#\1) . "")) (parse maybe-some-digits "1"))
    (is equal '((() . "a")) (parse maybe-some-digits "a"))
    (is equal '(((#\1 #\2 #\3) . "")) (parse maybe-some-digits "123"))
    (is equal '((() . "abc")) (parse maybe-some-digits "abc"))
    (is equal '(((#\1 #\2 #\3) . "abc")) (parse maybe-some-digits "123abc"))))

(define-test @one-or-more
  (let ((some-digits (@one-or-more (@digit))))
    (is equal '() (parse some-digits ""))
    (is equal '(((#\1) . "")) (parse some-digits "1"))
    (is equal '() (parse some-digits "a"))
    (is equal '(((#\1 #\2 #\3) . "")) (parse some-digits "123"))
    (is equal '() (parse some-digits "abc"))
    (is equal '(((#\1 #\2 #\3) . "abc")) (parse some-digits "123abc"))))

(define-test @digit
  (is equal '() (parse (@digit) ""))
  (is equal '((#\1 . "")) (parse (@digit) "1"))
  (is equal '() (parse (@digit) "a"))
  (is equal '((#\1 . "23")) (parse (@digit) "123"))
  (is equal '() (parse (@digit) "abc"))
  (is equal '((#\1 . "23abc")) (parse (@digit) "123abc")))

(define-test @lower
  (is equal '() (parse (@lower) ""))
  (is equal '() (parse (@lower) "A"))
  (is equal '((#\a . "")) (parse (@lower) "a"))
  (is equal '() (parse (@lower) "ABC"))
  (is equal '((#\a . "bc")) (parse (@lower) "abc"))
  (is equal '() (parse (@lower) "ABCabc")))

(define-test @upper
  (is equal '() (parse (@upper) ""))
  (is equal '((#\A . "")) (parse (@upper) "A"))
  (is equal '() (parse (@upper) "a"))
  (is equal '((#\A . "BC")) (parse (@upper) "ABC"))
  (is equal '() (parse (@upper) "abc"))
  (is equal '((#\A . "BCabc")) (parse (@upper) "ABCabc")))

(define-test @letter
  (is equal '() (parse (@letter) ""))
  (is equal '() (parse (@letter) "1"))
  (is equal '((#\a . "")) (parse (@letter) "a"))
  (is equal '((#\A . "")) (parse (@letter) "A"))
  (is equal '() (parse (@letter) "123"))
  (is equal '((#\a . "bc")) (parse (@letter) "abc"))
  (is equal '((#\A . "BC")) (parse (@letter) "ABC"))
  (is equal '() (parse (@letter) "123abc"))
  (is equal '((#\a . "bc123")) (parse (@letter) "abc123"))
  (is equal '((#\A . "BC123")) (parse (@letter) "ABC123")))

(define-test @alphanum
  (is equal '() (parse (@alphanum) ""))
  (is equal '((#\1 . "")) (parse (@alphanum) "1"))
  (is equal '((#\a . "")) (parse (@alphanum) "a"))
  (is equal '((#\A . "")) (parse (@alphanum) "A"))
  (is equal '((#\1 . "23")) (parse (@alphanum) "123"))
  (is equal '((#\a . "bc")) (parse (@alphanum) "abc"))
  (is equal '((#\A . "BC")) (parse (@alphanum) "ABC"))
  (is equal '((#\1 . "23abc")) (parse (@alphanum) "123abc"))
  (is equal '((#\a . "bc123")) (parse (@alphanum) "abc123"))
  (is equal '((#\A . "BC123")) (parse (@alphanum) "ABC123")))

(define-test @char
  (is equal '() (parse (@char #\-) ""))
  (is equal '() (parse (@char #\-) "1"))
  (is equal '() (parse (@char #\-) "a"))
  (is equal '() (parse (@char #\-) "_"))
  (is equal '((#\- . "")) (parse (@char #\-) "-"))
  (is equal '((#\- . "1")) (parse (@char #\-) "-1"))
  (is equal '((#\- . "123")) (parse (@char #\-) "-123")))

(define-test @string
  (is equal '() (parse (@string "let") ""))
  (is equal '() (parse (@string "let") "l"))
  (is equal '() (parse (@string "let") "le"))
  (is equal '(("let" . "")) (parse (@string "let") "let"))
  (is equal '() (parse (@string "let") "leet"))
  (is equal '(("let" . "me")) (parse (@string "let") "letme"))
  (is equal '(("let" . " x = 42;")) (parse (@string "let") "let x = 42;")))
