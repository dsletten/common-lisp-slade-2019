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
(load "/home/slytobias/lisp/packages/test.lisp")

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

;;;
;;;    Slade's solution to 14.8.4 (Typos fixed)
;;;    
(defclass 3d-array ()
  ((x-dim :initarg :x-dim :accessor x-dim)
   (y-dim :initarg :y-dim :accessor y-dim)
   (z-dim :initarg :z-dim :accessor z-dim)
   (the-array :accessor the-array))
  (:documentation "Slade lame three-dimensional array."))

(defun make-3d-array (x y z)
  (let ((new-array (make-instance '3d-array :x-dim x :y-dim y :z-dim z)))
    (setf (the-array new-array) (make-array x))
    (dotimes (count-y x new-array)
      (setf (aref (the-array new-array) count-y) (make-array y))
      (dotimes (count-z y)
        (setf (aref (aref (the-array new-array) count-y) count-z) (make-array z)))) ))

(defun 3d-array-p (obj)
  (typep obj '3d-array))

(defmethod print-object ((obj 3d-array) stream)
  (format stream "#<3d-array (~Ax~Ax~A) ~A>" (x-dim obj) (y-dim obj) (z-dim obj) (the-array obj)))

(defmethod 3d-ref ((obj 3d-array) (x integer) (y integer) (z integer))
  (cond ((or (minusp x) (>= x (x-dim obj))) (error "3d-ref -- X value out of range: ~D" x))
        ((or (minusp y) (>= y (y-dim obj))) (error "3d-ref -- Y value out of range: ~D" y))
        ((or (minusp z) (>= z (z-dim obj))) (error "3d-ref -- Z value out of range: ~D" z))
        (t (aref (aref (aref (the-array obj) x) y) z))))

;;;
;;;    No bounds checks!
;;;    
(defmethod (setf 3d-ref) (val (obj 3d-array) (x integer) (y integer) (z integer))
  (setf (aref (aref (aref (the-array obj) x) y) z) val))

(defmethod 3d-array-fill ((obj 3d-array) val)
  (map 'vector #'(lambda (d2)
                   (map 'vector #'(lambda (v) (fill v val)) d2))
       (the-array obj)))

;;;
;;;    Slade's solution to 14.8.5 (Typos fixed)
;;;    
(defclass nd-array ()
  ((dimensions-of :initarg :dimensions-of :accessor dimensions-of) ; Why accessor??
   (the-array :accessor the-array))
  (:documentation "Slade's lame N-dimensional array."))

(defun nd-array-p (obj)
  (typep obj 'nd-array))

(defun make-nd-array (d-list) ; Why not &rest ??
  ;; (let ((dim (copy-list d-list)) ; ???
  ;;       (nd-array (make-instance 'nd-array :dimensions-of d-list)))
  ;; (setf (the-array nd-array) (nd-init nil dim))
  (let ((nd-array (make-instance 'nd-array :dimensions-of d-list)))
    (setf (the-array nd-array) (nd-init nil d-list))
    nd-array))

(defun nd-init (vector n)
  (cond ((null n) vector)
        ((null vector) (nd-init (make-array (car n)) n))
        ((or (= (car n) 0) (null (cdr n))) vector)
        (t (setf (aref vector (- (car n) 1)) (nd-init nil (cdr n)))
           (nd-init vector (cons (- (car n) 1) (cdr n)))) )) ; Good grief!

(defmethod print-object ((obj nd-array) stream)
  (format stream "#<nd-array ~A ~A>" (dimensions-of obj) (the-array obj)))

;;;
;;;    No bounds checks!
;;;    
(defmethod nd-ref ((obj nd-array) &rest d)
  (dref (the-array obj) d))

(defun dref (array dimensions) ; Not dimensions! Indexes...
  (cond ((null (cdr dimensions)) (aref array (car dimensions)))
        (t (dref (aref array (car dimensions)) (cdr dimensions)))) )

(defmethod (setf nd-ref) (val (obj nd-array) &rest d)
  (setf (dref (the-array obj) d) val))

(defun (setf dref) (val array dimensions)
  (cond ((null (cdr dimensions)) (setf (aref array (car dimensions)) val))
        (t (setf (dref (aref array (car dimensions)) (cdr dimensions)) val))))

(defmethod map-nd-array ((self nd-array) proc) ; Ah. It's SELF now...
  (nd-map proc (the-array self) (length (dimensions-of self)))) ; Returns vector of nested vectors _not_ ND-ARRAY object! No (inherent side effect...)

(defmethod nd-array-fill ((self nd-array) val)
  (nd-map #'(lambda (v) (fill v val)) (the-array self) (- (length (dimensions-of self)) 1))) ; Returns vector of nested vectors _not_ ND-ARRAY object! FILL causes side effect to SELF!

(defun nd-map (func obj dim)
  (cond ((zerop dim) (funcall func obj))
        (t (map 'vector #'(lambda (v) (nd-map func v (- dim 1))) obj))))
