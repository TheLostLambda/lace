(defpackage llace/tests/main
  (:use :cl
        :llace
        :llace/parsing-primatives
        :rove))
(in-package :llace/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :llace)' in your Lisp.

(deftest item-parser
  (testing "Parsing an item from an empty string"
    (ng (item "")))
  (testing "Parsing an item from a non-empty string"
    (ok (equal (item "sup") '((#\s . "up"))))))
