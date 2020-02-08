;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a language for doing what you've been told is impossible.
;;;;   -- Kent Pitman
;;;;
;;;;   Name:               persistent-queue.lisp
;;;;
;;;;   Started:            Tue Dec  3 03:15:23 2019
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
;;;;   Example: Functional (immutable) queue data structure. Inspired by Oz (CTMCP).
;;;;   Values are enqueued to head of end of queue which is reversed as needed to allow dequeuing.
;;;;
;;;;   Notes:
;;;;
;;;;
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :persistent-queue (:use :common-lisp :test))

(in-package :persistent-queue)

;;
;;    Size??
;;    
(defun make-queue (head tail)
  (list head tail))

(defun make-empty-queue ()
  (make-queue '() '()))

(defun head (q)
  (first q))

(defun tail (q)
  (second q))

(defun emptyp (q)
  (and (null (head q)) (null (tail q))))

(defun enqueue (q obj)
  (make-queue (head q) (cons obj (tail q))))

(defun dequeue (q)
  (cond ((emptyp q) (error "Queue is empty."))
        ((null (head q)) (let ((queue (reverse (tail q))))
                           (values (first queue) (make-queue (rest queue) '()))) )
        (t (values (first (head q)) (make-queue (rest (head q)) (tail q)))) ))

(defun load-queue (&rest values)
  (do ((q (make-empty-queue) (enqueue q (first vals)))
       (vals values (rest vals)))
      ((endp vals) q)))

(defun process-queue (q)
  (if (emptyp q)
      q
      (multiple-value-bind (obj q) (dequeue q)
        (format t "Next element: ~A~%" obj)
        (process-queue q))))

(deftest test-queue ()
  (let* ((elts '(a b c d))
         (q (apply #'load-queue elts)))
    (emptyp (process-queue q))
    (emptyp (process-queue q))))



