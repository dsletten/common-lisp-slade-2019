;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Pascal is for building pyramids—imposing, breathtaking, static structures built by armies pushing heavy blocks into place. Lisp is for building organisms...
;;;;   -- Alan Perlis
;;;;
;;;;   Name:               quiz.lisp
;;;;
;;;;   Started:            Wed May 13 23:12:19 2020
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;       Fixed Slade's version (Exercise 14.8.16)
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
(load "/Users/dsletten/lisp/books/Slade/2019/ch14/random-generator.lisp")

(defpackage :quiz (:use :common-lisp :test :random-generator) (:shadowing-import-from :random-generator :random))

(in-package :quiz)

(defclass problem ()
  ((operator :reader operator :initarg :operator)
   (op1 :reader op1 :initarg :op1)
   (op2 :reader op2 :initarg :op2)
   (operation :reader operation)))

(defgeneric invoke (problem))
(defmethod invoke ((p problem))
  (funcall (operator p) (op1 p) (op2 p)))

(defun make-problem (operator op1 op2)
  (ccase operator
    (+ (make-instance 'addition-problem :op1 op1 :op2 op2))
    (- (make-instance 'subtraction-problem :op1 op1 :op2 op2))
    (* (make-instance 'multiplication-problem :op1 op1 :op2 op2))))

(defclass addition-problem (problem) ())

(defmethod initialize-instance :after ((p addition-problem) &rest initargs)
  (declare (ignore initargs))
  (with-slots (operator operation) p
    (setf operator #'+
          operation "plus")))

(defclass multiplication-problem (problem) ())

(defmethod initialize-instance :after ((p multiplication-problem) &rest initargs)
  (declare (ignore initargs))
  (with-slots (operator operation) p
    (setf operator #'*
          operation "times")))

(defclass subtraction-problem (problem) ())

(defmethod initialize-instance :after ((p subtraction-problem) &rest initargs)
  (declare (ignore initargs))
  (with-slots (operator op1 op2 operation) p
    (when (< op1 op2)
      (rotatef op1 op2))
    (setf operator #'-
          operation "minus")))

(defun quiz (f size)
  (let ((rng (make-random size))
        (right 0)
        (wrong 0))
    (labels ((generate-problem (&optional previous-problem)
               (let ((problem (make-problem f (get-random-operand) (get-random-operand))))
                 (if (and previous-problem (duplicate-problem-p problem previous-problem))
                     (generate-problem previous-problem)
                     (ask-question problem :initial t))))
             (get-random-operand () (random rng))
             (duplicate-problem-p (new old)
               (with-slots ((new-1 op1) (new-2 op2)) new
                 (with-slots ((old-1 op1) (old-2 op2)) old
                   (and (= new-1 old-1) (= new-2 old-2)))) )
             (ask-question (problem &key initial)
               (with-slots (operation op1 op2) problem
                 (format *query-io* "How much is ~D ~A ~D? " op1 operation op2)
                 (force-output *query-io*)
                 (let ((response (read)))
                   (cond ((member response '(q quit stop exit)) nil)
                         ((and (numberp response) (= response (invoke problem)))
                          (when initial
                            (incf right))
                          (right-reply)
                          (generate-problem problem))
                         (t (when initial
                              (incf wrong))
                            (wrong-reply)
                            (ask-question problem :initial nil)))) )))
      (generate-problem)
      (print-score right wrong))))

(defun make-reply (replies)
  (let ((rng (make-random (length replies))))
    #'(lambda ()
        (format t "~A~2%" (aref replies (random rng)))) ))

(setf (symbol-function 'right-reply)
      (make-reply #("Right!" "OK. That's good." "Just what I would have said!" "Close enough. (In fact, exactly right.)" "Great! Super! Let's keep going..." "Of course! (Why didn't I think of that?)" "Yep. Nice work.")))

(setf (symbol-function 'wrong-reply)
      (make-reply #("In a word: no." "Not quite right. One more time." "Try again. You can get it right.")))

(defun print-score (right wrong)
  (format t "First tries: ~D correct out of ~D.~%" right (+ right wrong)))

