;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               random-generator.lisp
;;;;
;;;;   Started:            Sat May  9 22:54:09 2020
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

(defpackage :random-generator
  (:use :common-lisp :test)
  (:shadow :random)
  (:export :make-random :random :seed :range :generator))

(in-package :random-generator)

(defclass rng ()
  ((seed :initarg :seed :reader seed :documentation "Initial random state")
   (range :initarg :range :reader range :documentation "Upper range of random number")
   (generator :reader generator :documentation "Random function"))
  (:documentation "A random number generator"))

(defun make-random (&optional (range 100) (seed (mod (get-universal-time) range)))
  (make-instance 'rng :seed seed :range range))

(defmethod initialize-instance :after ((rng rng) &rest initargs)
  (declare (ignore initargs))
  (let ((modulus (expt 2 16))
        (multiplier 25173)
        (increment 13849))
    (with-slots (seed range generator) rng
      (flet ((convert (n)
               (typecase range
                 (integer (floor n))
                 (float (float n range))
                 (otherwise n))))
        (setf generator #'(lambda ()
                            (setf seed (mod (+ increment (* multiplier seed)) modulus))
                            (convert (/ (* seed range) modulus)))) ))))
                        
(defgeneric random (rng)
  (:documentation "Retrieve random number from the generator."))
(defmethod random ((rng rng))
  (values (funcall (generator rng))))

;;;
;;;    Only works for integer RNGs!
;;;    
(defun test-random (rng n)
  (let ((trials (make-array (range rng) :initial-element 0)))
    (loop repeat n 
          do (incf (aref trials (random rng)))
          finally (return trials))))

;;;
;;;    Ex. 14.8.11/12
;;;    
(defun chi-squared (trials &optional (expected (make-array (length trials) :initial-element (/ (reduce #'+ trials) (length trials)))) )
  (flet ((square-diff (x y)
           (let ((diff (- x y)))
             (* diff diff))))
    (coerce (loop for bin across trials
                  for expect across expected
                  summing (/ (square-diff bin expect) expect)) 'double-float)))

;; (defun chi-squared (trials)
;;   (let ((expected (/ (reduce #'+ trials) (length trials))))
;;     (flet ((square-diff (x y)
;;              (let ((diff (- x y)))
;;                (* diff diff))))
;;       (coerce (loop for bin across trials summing (/ (square-diff bin expected) expected)) 'double-float))))

(defun test-cl-random (range n)
  (let ((trials (make-array range :initial-element 0))
        (random-state (make-random-state t)))
    (loop repeat n 
          do (incf (aref trials (cl:random range random-state)))
          finally (return trials))))
