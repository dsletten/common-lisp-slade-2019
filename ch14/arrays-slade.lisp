;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is the medium of choice for people who enjoy free style and flexibility.
;;;;   -- Gerald Jay Sussman
;;;;
;;;;   Name:               arrays-slade.lisp
;;;;
;;;;   Started:            Sun May 17 06:53:42 2020
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
;(load "/home/slytobias/lisp/packages/test.lisp")
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :arrays-slade (:use :common-lisp :test))

(in-package :arrays-slade)

(defclass 2d-array ()
  ((rows :initarg :rows :reader rows)
   (columns :initarg :columns :reader columns)
   (the-array :accessor the-array))
  (:documentation "Slade's flaky two-dimensional array."))

(defun make-2d-array (x y)
  (let ((new-array (make-instance '2d-array :rows x :columns y)))
    (setf (the-array new-array) (make-array x))
    (dotimes (count x new-array)
      (setf (aref (the-array new-array) count) (make-array y)))) )

(defun 2d-array-p (obj)
  (typep obj '2d-array))

(defmethod print-object ((obj 2d-array) stream)
  (format stream "<2d-array (~Ax~A) ~A>" (rows obj) (columns obj) (the-array obj)))

;; (defmethod 2d-ref ((obj 2d-array) (x integer) (y integer))
;;   (aref (aref (the-array obj) x) y))

(defmethod 2d-ref ((obj 2d-array) (x integer) (y integer))
  (assert (and (not (minusp x)) (< x (rows obj)))
          (x)
          "2d-ref error. row value: ~D is out of range (0-~D)" x (rows obj))
  (assert (and (not (minusp y)) (< y (columns obj)))
          (y)
          "2d-ref error. column value: ~D is out of range (0-~D)" y (columns obj))
  (aref (aref (the-array obj) x) y))

(defmethod (setf 2d-ref) (val (obj 2d-array) (x integer) (y integer))
  (setf (aref (aref (the-array obj) x) y) val))

(defmethod 2d-array-fill ((obj 2d-array) val)
  (map nil #'(lambda (v) (fill v val)) (the-array obj)))
