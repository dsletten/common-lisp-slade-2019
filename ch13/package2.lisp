;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a language for doing what you've been told is impossible.
;;;;   -- Kent Pitman
;;;;
;;;;   Name:               package2.lisp
;;;;
;;;;   Started:            Tue Sep 10 23:59:08 2019
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

(defpackage :package2 (:use :common-lisp :test))

(in-package :package2)

(defclass pung ()
  ((foo :initarg :foo :reader foo)
   (baz :initform "classy" :allocation :class)))

(defmethod print-object ((pung pung) stream)
  (print-unreadable-object (pung stream :type t)
    (format stream " New PUNG ~A" (foo pung))))

