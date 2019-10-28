;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a language for doing what you've been told is impossible.
;;;;   -- Kent Pitman
;;;;
;;;;   Name:               package1.lisp
;;;;
;;;;   Started:            Tue Sep 10 23:59:06 2019
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

(defpackage :package1 (:use :common-lisp :test))

(in-package :package1)

(defclass pung ()
  ((foo :initarg :foo :reader foo)
   (bar :initarg :bar :reader bar)))

(defmethod print-object ((pung pung) stream)
  (print-unreadable-object (pung stream :type t)
    (format stream " Old PUNG ~A ~A" (foo pung) (bar pung))))
