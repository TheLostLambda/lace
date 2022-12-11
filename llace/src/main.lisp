(defpackage llace
  (:use :cl :llace/scanner)
  (:export :repl))
(in-package :llace)

(defun repl ()
  (loop (lc-print (lc-eval (lc-read)))))

(defun lc-print (x) x)
(defun lc-eval (x) x)

(defun lc-read ()
  (scan (read-line)))

(defun scan (str)
  (print str)
  str)

