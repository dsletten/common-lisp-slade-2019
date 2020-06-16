;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Pascal is for building pyramids—imposing, breathtaking, static structures built by armies pushing heavy blocks into place. Lisp is for building organisms...
;;;;   -- Alan Perlis
;;;;
;;;;   Name:               hash-slade.lisp
;;;;
;;;;   Started:            Sat May 23 05:31:23 2020
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
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :hash-slade (:use :common-lisp :test) (:shadow :hash-table :hash-table-p :make-hash-table :get))

(in-package :hash-slade)

(defclass hash-table ()
  ((size :initform 10 :initarg :size :accessor size :documentation "Size of hash table")
   (table :initform nil :accessor table :documentation "Vector of values"))
  (:documentation "Slade-like hash table class"))

(defgeneric hash-table-p (obj)
  (:method ((obj hash-table)) t)
  (:method (obj) nil))

(defun make-hash-table (size)
  (let ((hash-table (make-instance 'hash-table :size size)))
    (setf (table hash-table) (make-array size :initial-element nil))
    hash-table))

(defmethod reveal ((obj hash-table))
  (table obj))

(defmethod next-number ((table hash-table) (n integer))
  (incf n)
  (cond ((= n (size table)) 0)
        (t n)))

(defmethod get-bucket ((table hash-table) (n integer))
  (aref (table table) n))

;;;
;;;    This ends up in an infinite loop when the table is full when no entry exists for KEY:
;;;    1. Trying to PUT an entry for KEY into the table.
;;;    2. Trying to GET an entry for a non-existent key.
;;;    
(defmethod get-number ((table hash-table) key n)
  (when (null n)  ; ?!!!
    (setf n (mod (sxhash key) (size table))))
  (let ((bucket (get-bucket table n)))
    (cond ((null bucket) n)
          ((eq key (car bucket)) n)
          (t (get-number table key (next-number table n)))) )) ; Possible infinite loop!

(defmethod get ((table hash-table) key)
  (cdr (get-bucket table (get-number table key nil)))) ; ????

(defmethod put ((table hash-table) key value)
  (setf (aref (table table) (get-number table key nil)) ; ???
        (cons key value)))

