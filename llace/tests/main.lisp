(defpackage llace/tests/main
  (:use :cl
        :llace
        :rove))
(in-package :llace/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :llace)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
