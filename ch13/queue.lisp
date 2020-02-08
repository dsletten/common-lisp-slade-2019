;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               queue.lisp
;;;;
;;;;   Started:            Mon Nov  4 02:29:41 2019
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

(defpackage :queue (:use :common-lisp :test))

(in-package :queue)

(defclass queue ()
  ((head :type cons)
   (tail :type cons)
   (size :initform 0 :reader size :type (integer 0))))

(defmethod initialize-instance :after ((q queue) &rest initargs)
  (let ((tconc (cons nil nil)))
    (with-slots (head tail) q
      (setf head tconc tail tconc))))

(defmethod print-object ((q queue) stream)
  (print-unreadable-object (q stream :type t)
    (format stream "[~D]" (size q))))

(defgeneric emptyp (q)
  (:documentation "Is the queue Q empty?"))
(defmethod emptyp ((q queue))
  (with-slots (head tail) q
    (eq head tail)))

(defgeneric enqueue (q obj)
  (:documentation "Place OBJ at the end of the queue Q."))
(defmethod enqueue ((q queue) obj)
  (let ((tconc (cons nil nil)))
    (with-slots (tail size) q
      (setf (car tail) obj
            (cdr tail) tconc
            tail tconc)
      (incf size))))

(defgeneric dequeue (q)
  (:documentation "Remove and return the element at the head of queue Q."))
(defmethod dequeue ((q queue))
  (if (emptyp q)
      (error "Queue is already empty.")
      (with-slots (head size) q
        (decf size)
        (pop head))))
        ;; (let ((elt (car head)))
        ;;   (setf head (cdr head))
        ;;   (decf size)
        ;;   elt))))

(defun process-queue (queue)
  (unless (emptyp queue)
    (format t "Next element: ~A~%" (dequeue queue))
    (process-queue queue)))

(deftest test-queue ()
  (let ((q (make-instance 'queue))
        (elts '(a b c d)))
    (dolist (elt elts)
      (enqueue q elt))
    (check
     (= (size q) (length elts))
     (progn (loop repeat (length elts)
                  do (dequeue q))
            (emptyp q)))) )

