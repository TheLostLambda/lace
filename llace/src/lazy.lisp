(defpackage llace/lazy
  (:use :cl :serapeum/bundle)
  (:export :deflazy))
(in-package :llace/lazy)

(defmacro deflazy (name lambda-list &body body)
  (let ((strict-name (intern (fmt "!~A" name))))
    `(eval-always
       (clazy:deflazy ,strict-name ,lambda-list ,@body)
       (defmacro ,name ,lambda-list
         (list 'clazy:call '',strict-name ,@lambda-list)))))
