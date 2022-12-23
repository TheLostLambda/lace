(defpackage llace/tests/functional-parsing
  (:use :cl :parachute :llace/functional-parsing))
(in-package :llace/tests/functional-parsing)

(define-test item-parser
  (is equal '() (parse (item) ""))
  (is equal '((#\s . "up")) (parse (item) "sup")))
