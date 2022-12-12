(defpackage llace/tests/main
  (:use :cl
        :llace
        :llace/tests/parsing-primatives
        :rove))
(in-package :llace/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :llace)' in your Lisp.

;; TODO: Move to FiveAM for testing instead of Rove? (See here:
;; https://lispcookbook.github.io/cl-cookbook/testing.html)

(run-suite :llace/tests/parsing-primatives)
