;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               ch05.lisp
;;;;
;;;;   Started:            Mon Jul 29 22:35:33 2019
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

(defpackage :ch05 (:use :common-lisp :test) (:shadow :count))

(in-package :ch05)

;;;
;;;    5.7.7
;;;
;; (let ((count 0)
;;       (args '()))
;;   (defun a+ (x y)
;;     (incf count)
;;     (push (list x y) args)
;;     (+ x y))
;;   (defun count+ ()
;;     (let ((old-count count))
;;       (setf count 0)
;;       old-count))
;;   (defun args+ ()
;;     (let ((old-args args))
;;       (setf args '())
;;       old-args)))

(let ((count 0)
      (args '()))
  (defun a+ (x y)
    (incf count)
    (push (list x y) args)
    (+ x y))
  (defun count+ ()
    (prog1 count
      (setf count 0)))
  (defun args+ ()
    (prog1 args
      (setf args '()))) )

(defclass counter ()
  ((count :accessor count :initform 0)
   (args :accessor args :initform '())))

(defgeneric add (counter x y))
(defmethod add ((c counter) x y)
  (incf (count c))
  (push (list x y) (args c))
  (+ x y))

(defgeneric reset (counter))
(defmethod reset ((c counter))
  (setf (count c) 0
        (args c) '()))

(defmethod print-object ((c counter) stream)
  (print-unreadable-object (c stream :type t :identity t)
    (format stream "Counter count: ~A args: ~A" (count c) (args c))))

;;;
;;;    5.7.8
;;;
(defun make-date (month day year)
  (cons month (cons day (cons year '()))) )

(defun date-month (date)
  (car date))

(defun date-day (date)
  (cadr date))

(defun date-year (date)
  (caddr date))

(defun (setf date-month) (month date)
  (setf (car date) month))

(defun (setf date-day) (day date)
  (setf (cadr date) day))

(defun (setf date-year) (year date)
  (setf (caddr date) year))

(defclass date ()
  ((month :accessor month :initarg :month :type (integer 1 12))
   (day :accessor day :initarg :day :type (integer 1 31))
   (year :accessor year :initarg :year :type (integer 0))))

(defmethod print-object ((d date) stream)
  (print-unreadable-object (d stream :type t :identity t)
    (format stream "month: ~A day: ~A year: ~A" (month d) (day d) (year d))))


