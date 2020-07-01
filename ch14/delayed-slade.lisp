;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Pascal is for building pyramids—imposing, breathtaking, static structures built by armies pushing heavy blocks into place. Lisp is for building organisms...
;;;;   -- Alan Perlis
;;;;
;;;;   Name:               delayed-slade.lisp
;;;;
;;;;   Started:            Sun Jun 21 00:27:30 2020
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

(defpackage :delayed-slade
  (:use :common-lisp :test)
  (:export :delayed-p :delay :force))

(in-package :delayed-slade)

(defclass delayed-state ()
  ((next :initform nil :initarg :next :accessor next :documentation "generator function to produce the next value"))
  (:documentation "a class for delayed evaluation"))

(defmethod print-object ((self delayed-state) stream)
  (format stream "#<Delayed>"))

(defun delayed-p (obj)
  (typep obj 'delayed-state))

(defmacro delay (form)
  `(let ((new-state (make-instance 'delayed-state)))
     (setf (next new-state) #'(lambda () ,form))
     new-state))

(defmethod force ((obj delayed-state))
  (funcall (next obj)))

(defun integers2 (n)
  (cons n (delay (integers2 (1+ n)))) )

(defmacro lazy-pop (n)
  `(prog1
       (car ,n)
     (setf ,n (force (cdr ,n)))) )

(defun odd-numbers (n)
  (cons n (delay (odd-numbers (+ n 2)))) )

(defclass lazy-2d-array ()
  ((rows :initarg :rows :accessor rows)
   (columns :initarg :columns :accessor columns)
   (fill-value :initform nil :accessor fill-value)
   (fill-flag :initform nil :accessor fill-flag)
   (the-array :accessor the-array))
  (:documentation "A two-dimensional lazy array."))

(defun make-lazy-2d-array (x y)
  (let ((new-array (make-instance 'lazy-2d-array :rows x :columns y)))
    (setf (the-array new-array) (make-array x))
    (dotimes (count x new-array)
      (setf (aref (the-array new-array) count) (delay (make-array y)))) ))

(defun lazy-2d-array-p (obj)
  (typep obj 'lazy-2d-array))

(defmethod print-object ((obj lazy-2d-array) stream)
  (format stream "<lazy-2d-array (~Ax~A) ~A>" (rows obj) (columns obj) (the-array obj)))

(defmethod lazy-2d-ref ((obj lazy-2d-array) (x integer) (y integer))
  (assert (and (not (minusp x)) (< x (rows obj))) (x) "lazy-2d-ref error. row value: ~D is out of range (0-~D)" x (1- (rows obj)))
  (assert (and (not (minusp y)) (< y (columns obj))) (y) "lazy-2d-ref error. column value: ~D is out of range (0-~D)" y (1- (columns obj)))
  (aref (lref obj x) y))

;;; :before method!
(defmethod lref ((obj lazy-2d-array) (x integer))
  (when (delayed-p (aref (the-array obj) x))
    (setf (aref (the-array obj) x) (force (aref (the-array obj) x)))
    (when (fill-flag obj)
      (fill (aref (the-array obj) x) (fill-value obj))))
  (aref (the-array obj) x))

;;;    No bounds check?!?
(defmethod (setf lazy-2d-ref) (val (obj lazy-2d-array) (x integer) (y integer))
  (setf (aref (lref obj x) y) val))

(defmethod lazy-2d-array-fill ((obj lazy-2d-array) val)
  (map 'vector #'(lambda (v)
                   (cond ((delayed-p v) (setf (fill-flag obj) t
                                              (fill-value obj) val))
                         (t (fill v val))))
       (the-array obj)))
