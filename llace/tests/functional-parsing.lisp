(defpackage llace/tests/functional-parsing
  (:use :cl :serapeum :parachute :llace/functional-parsing)
  (:shadowing-import-from :parachute :true))
(in-package :llace/tests/functional-parsing)

(define-test item
  (is equal '() (parse (item) ""))
  (is equal '((#\s . "up")) (parse (item) "sup")))

(define-test >>=
  (let ((double-char (>>= (item) (op (is-char _)))))
    (is equal '() (parse double-char ""))
    (is equal '() (parse double-char "a"))
    (is equal '() (parse double-char "ab"))
    (is equal '((#\a . "")) (parse double-char "aa"))
    (is equal '((#\a . "bb")) (parse double-char "aabb"))
    (finish (parse (>>= (item) (loop)) ""))))

(define-test >>
  (let ((digit-item (>> (digit) (item))))
    (is equal '() (parse digit-item ""))
    (is equal '() (parse digit-item "4"))
    (is equal '() (parse digit-item "b"))
    (is equal '() (parse digit-item "b4"))
    (is equal '((#\a . "")) (parse digit-item "4a"))
    (is equal '((#\b . "")) (parse digit-item "4b"))
    (is equal '((#\a . "bc")) (parse digit-item "4abc"))
    (finish (parse (>> (item) (loop)) ""))))

(define-test constant
  (is equal '((42 . "")) (parse (constant 42) ""))
  (is equal '((42 . "foo")) (parse (constant 42) "foo")))

(define-test nothing
  (is equal '() (parse (nothing) ""))
  (is equal '() (parse (nothing) "foo")))

(define-test either
  (let ((digit-or-item (either (digit) (item))))
    (is equal '() (parse digit-or-item ""))
    (is equal '((#\4 . "")) (parse digit-or-item "4"))
    (is equal '((#\b . "")) (parse digit-or-item "b"))
    (is equal '((#\b . "4")) (parse digit-or-item "b4"))
    (is equal '((#\4 . "a")) (parse digit-or-item "4a"))
    (is equal '((#\4 . "abc")) (parse digit-or-item "4abc"))
    (finish (parse (either (item) (loop)) "1"))))
