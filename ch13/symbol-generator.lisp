;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               symbol-generator.lisp
;;;;
;;;;   Started:            Mon Sep  9 22:09:00 2019
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;
;;;;
;;;;
;;;;   Calling Sequence:
;;;;
;;;;
;;;;   Inputs:
;;;;
;;;;   Outputs:
;;;;
;;;;   Example:
;;;;
;;;;   Notes:
;;;;
;;;;
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :symbol-generator (:use :common-lisp :test))

(in-package :symbol-generator)

(defclass symbol-generator ()
  ((prefix :initarg :prefix :initform "G" :reader prefix)
   (counter :initform 0 :accessor counter)
   (cache :initform '() :accessor cache)))

(defmethod print-object ((generator symbol-generator) stream)
  (print-unreadable-object (generator stream :type t)
    (format stream "prefix: \"~A\" counter: ~D" (prefix generator) (counter generator))))

(defgeneric generate (generator)
  (:documentation "Generate a sequence of symbols with a given prefix."))
(defmethod generate ((generator symbol-generator))
  (let ((symbol (read-from-string (format nil "~A~4,'0D" (prefix generator) (counter generator)))) )
    (push symbol (cache generator))
    (incf (counter generator))
    symbol))

(defgeneric last-symbol (generator)
  (:documentation "Return the last symbol produced by a symbol generator."))
(defmethod last-symbol ((generator symbol-generator))
  (first (cache generator)))

(defgeneric reset (generator)
  (:documentation "Restore a symbol generator to its initial state."))
(defmethod reset ((generator symbol-generator))
  (setf (counter generator) 0 (cache generator) '()))

(defmacro defgenerator (name)
  `(progn
     (when (boundp ',name)
       (makunbound ',name))
     (defvar ,name (make-instance 'symbol-generator :prefix ',name))
     (defun ,name () (generate ,name))))
