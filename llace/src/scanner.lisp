(defpackage llace/scanner
  (:use :cl)
  (:export :make-scanner))
(in-package :llace/scanner)

;; TODO: Maybe consider adding types here?
;; Also taking the OOP approach here, but I could do this functionally using
;; some sort of automaton
;; TODO: Change this to be a struct!
(defclass scanner ()
  ((source :initarg :source)
   (tokens :initform '())
   (start :initform 0)
   (current :initform 0)
   (line :initform 1)
   (indent :initform 0)))

(defun make-scanner (source)
  (make-instance 'scanner :source source))

;; TODO: Add a constructor to this struct
(defstruct token
  type lexeme literal line indent)

(defmethod scan-tokens ((obj scanner))
  (with-slots (source tokens start current line indent) obj
    (loop :while (< current (length source))
          :do (setf start current)
              (scan-token obj))
    tokens))

(defmethod scan-token ((obj scanner))
  (with-slots (start current) obj
    (case (advance obj)
      ; Why can't methods have different numbers of arguments?
      (#\+ (add-token obj :+ nil))
      (otherwise (format t "PLACEHOLDER ERROR")))))

(defmethod advance ((obj scanner))
  (with-slots (source current) obj
    (char source (1- (incf current)))))

(defmethod add-token ((obj scanner) (type symbol) (literal t))
  (with-slots (source tokens start current line indent) obj
    (let ((lexeme (subseq source start current)))
      (push (make-token type lexeme literal line indent) tokens))))
