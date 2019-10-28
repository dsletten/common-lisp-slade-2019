;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               derive.lisp
;;;;
;;;;   Started:            Sat Sep 28 00:46:27 2019
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
;;;;   Notes: Experiments based on Alan Crowe's DERIVE-EVAL example.
;;;;
;;;;
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :derive (:use :common-lisp :test))

(in-package :derive)

;;;
;;;    Just a stub...
;;;    
(defun derive (expr var)
  `(* 2 ,var))

(defun derive-eval-1 (expr var val)
  (setf (symbol-value var) val)
  (eval (derive expr var)))

(defun cheating-derive-eval (expr var val)
;  (declare (ignore var))
  (let ((x val))
    (declare (special x))
    (eval (derive expr var))))

(defun derive-eval (expr var val)
  (progv (list var) (list val)
    (eval (derive expr var))))

(defmacro derive-evalm (expr var val)
  (let ((result (derive expr var)))
    `(let ((,var ,val))
       ,result)))

(defmacro bind-eval (vars vals expr)
  (let ((bindings (mapcar #'list vars vals)))
    `(let ,bindings
       ,expr)))
