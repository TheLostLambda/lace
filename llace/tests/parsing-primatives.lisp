(defpackage llace/tests/parsing-primatives
  (:use :cl
        :llace/parsing-primatives
        :rove))
(in-package :llace/tests/parsing-primatives)

(deftest item-parser
  (testing "Parsing an item from an empty string"
    (ng (parse (item) "")))
  (testing "Parsing an item from a non-empty string"
    (ok (equal (parse (item) "sup") '((#\s . "up"))))))

(deftest <$>-combinator
  (flet ((multi-item (input) (cons '(#\b . "ruh") (parse (item) input))))
    (let ((upper-item-parser (<$> #'char-upcase (item)))
          (upper-multi-item-parser (<$> #'char-upcase #'multi-item)))
      (testing "Mapping over a failed parse"
        (ng (parse upper-item-parser "")))
      (testing "Mapping over a single parse"
        (ok (equal (parse upper-item-parser "sup") '((#\S . "up")))))
      (testing "Mapping over multiple parses"
        (ok (equal (parse upper-multi-item-parser "sup")
                   '((#\B . "ruh") (#\S . "up"))))))))
