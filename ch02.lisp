;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               ch02.lisp
;;;;
;;;;   Started:            Sat Jul 20 02:43:46 2019
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

(defpackage :ch02 (:use :common-lisp :test))

(in-package :ch02)

;;;
;;;    2.11.2
;;;    
(defun add2 (x)
  (+ x 2))

(deftest test-add2 ()
  (check
   (= (add2 5) 7)
   (= (add2 -12) -10)
   (= (add2 0.4) 2.4)))

(defun add5 (x)
  (+ x 5))

(deftest test-add5 ()
  (check
   (= (add5 5) 10)
   (= (add5 -12) -7)
   (= (add5 0.4) 5.4)))

(defun double (x)
  (* x 2))

(deftest test-double ()
  (check
   (= (double 5) 10)
   (= (double -12) -24)
   (= (double 0.4) 0.8)))

(defun min-abs4 (a b c d)
  (min (abs a) (abs b) (abs c) (abs d)))

(deftest test-min-abs4 ()
  (check
   (= (min-abs4 3 5 -2 -8) 2)))

(defun min-abs (&rest args)
  (apply #'min (mapcar #'abs args)))

(deftest test-min-abs ()
  (check
   (= (min-abs 3) 3)
   (= (min-abs 3 5 -2) 2)
   (= (min-abs 3 5 -2 -8) 2)
   (= (min-abs 3 5 -2 -8 0.1) 0.1)))

(defun max-abs4 (a b c d)
  (max (abs a) (abs b) (abs c) (abs d)))

(deftest test-max-abs4 ()
  (check
   (= (max-abs4 3 5 -2 -8) 8)))

(defun max-abs (&rest args)
  (apply #'max (mapcar #'abs args)))

(deftest test-max-abs ()
  (check
   (= (max-abs 3) 3)
   (= (max-abs 3 5 -2) 5)
   (= (max-abs 3 5 -2 -8) 8)
   (= (max-abs 3 5 -2 -8 99) 99)))

