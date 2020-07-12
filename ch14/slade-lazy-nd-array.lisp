;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               slade-lazy-nd-array.lisp
;;;;
;;;;   Started:            Mon Jun 29 18:40:20 2020
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
(load "/home/slytobias/lisp/books/Slade/2019/ch14/delayed-slade.lisp")

(defpackage :slade-lazy-nd-array (:use :common-lisp :test :delayed-slade))

(in-package :slade-lazy-nd-array)

(defclass lazy-nd-array ()
  ((dimensions-of :initarg :dimensions-of :accessor dimensions-of) ; Why accessor?!
   (rank :initarg :rank :accessor rank) ; Why accessor?!
   (fill-value :initform nil :accessor fill-value)
   (fill-flag :initform nil :accessor fill-flag)
   (the-array :accessor the-array)) ; Great slot name!
  (:documentation "An N-dimensional lazy array."))

(defun make-lazy-nd-array (d-list)
  (let* ((rank (length d-list))
         (new-array (make-instance 'lazy-nd-array :rank rank :dimensions-of d-list)))
    (setf (the-array new-array) (lazy-nd-init nil d-list))
    new-array))

;;;
;;;    "N" is list of dimensions!
;;;    
(defun lazy-nd-init (vector n)
  (cond ((null n) vector)
        ((null vector) (lazy-nd-init (make-array (car n)) n))
        ((or (= (car n) 0) (null (cdr n))) vector) ; Simply return arg for 0-dimension or final dimension
        (t (setf (aref vector (- (car n) 1))
                 (delay (lazy-nd-init nil (cdr n)))) ; Set up next dimension for later.
           (lazy-nd-init vector (cons (- (car n) 1) (cdr n)))) )) ; SRSLY?! Recur for all elts of current dimension.

(defun lazy-nd-array-p (obj)
  (typep obj 'lazy-nd-array))

(defmethod print-object ((obj lazy-nd-array) stream)
  (format stream "<lazy-nd-array ~A ~A>" (dimensions-of obj) (the-array obj)))

(defmethod lazy-nd-ref ((obj lazy-nd-array) d)
  (dref obj (the-array obj) d))

;;;
;;;    "D" is list of indexes (not dimensions!)
;;;    ARRAY is initially top-level contents of OBJ. Recursive calls->lower dimensions
;;;    
(defmethod dref ((obj lazy-nd-array) array d)
  (cond ((delayed-p (aref array (car d)))
         (setf (aref array (car d)) (force (aref array (car d))))
         (if (null (cdr d))
             (fill-value obj)
             (dref obj array d))) ; ?! Superficially this looks like an infinite loop since
                                  ; "same" args are passed to DREF again! But the elt of ARRAY
                                  ; was a DELAY, and now it has been FORCEd. The result is that
                                  ; this will simply call A. below!!!!
        ((null (cdr d)) (aref array (car d))) 
        (t (dref obj (aref array (car d)) (cdr d)))) ) ; A.

(defmethod (setf lazy-nd-ref) (val (obj lazy-nd-array) d-list) ; !!
  (setf (dref obj (the-array obj) d-list) val))

(defmethod (setf dref) (val (obj lazy-nd-array) array d)
  (cond ((delayed-p (aref array (car d)))
         (setf (aref array (car d)) (force (aref array (car d)))) )
        ((null (cdr d))
         (setf (aref array (car d)) val))
        (t (setf (dref obj (aref array (car d)) (cdr d)) val))))

(defmethod lazy-nd-array-fill ((obj lazy-nd-array) val)
  (lazy-fill obj (the-array obj) val (cdr (dimensions-of obj))))

(defmethod lazy-fill ((obj lazy-nd-array) array val d-list)
  (cond ((delayed-p array) (setf (fill-flag obj) t
                                 (fill-value obj) val))
        ((cdr d-list) (map 'vector #'(lambda (v) (lazy-fill obj v val (cdr d-list))) array))
        (t (map 'vector
                #'(lambda (v) (cond ((delayed-p v)
                                     (setf (fill-flag obj) t ; Repeatedly set?!...
                                           (fill-value obj) val))
                                    (t (fill v val))))
                array))))
