;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Pascal is for building pyramids—imposing, breathtaking, static structures built by armies pushing heavy blocks into place. Lisp is for building organisms...
;;;;   -- Alan Perlis
;;;;
;;;;   Name:               quiz-slade.lisp
;;;;
;;;;   Started:            Sun May 10 20:07:27 2020
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;       Slade's basic version (more or less--already fixed some!!)
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

(defpackage :quiz-slade (:use :common-lisp :test :random-generator) (:shadowing-import-from :random-generator :random))

(in-package :quiz-slade)

(defun make-rng-vector (size)
  (let ((result (make-array size)))
    (dotimes (i size result)
      (setf (aref result i) (make-random (1+ i)))) ))

(defun quiz (f size)
  (let ((generators (make-rng-vector size))
        (function (symbol-function f))
        (operation (ccase f
                     (* "times")
                     (- "minus")
                     (+ "plus")))
        (right 0)
        (wrong 0))
    (labels ((small-operand (n)
               (random (aref generators n)))
             (large-operand ()
               (random (aref generators (1- size))))
             (make-problem (&optional (previous-big 0) (previous-small 0))
               (let* ((big (large-operand))
                      (small (small-operand big))
                      (answer (funcall function big small)))
                 (if (and (= big previous-big) (= small previous-small))
                     (make-problem previous-big previous-small)
                     (ask-question big small answer :initial t))))
             (print-score ()
               (format t "First tries: ~D correct out of ~D.~%" right (+ right wrong)))
             (ask-question (big small answer &key initial)
               (format *query-io* "How much is ~D ~A ~D? " big operation small)
               (force-output *query-io*)
               (let ((response (read)))
                 (cond ((member response '(q quit stop exit)) nil)
                       ((and (numberp response) (= response answer))
                        (when initial
                          (incf right))
                        (right-reply)
                        (make-problem big small))
                       (t (when initial
                            (incf wrong))
                          (wrong-reply)
                          (ask-question big small answer :initial nil)))) ))
      (make-problem)
      (print-score))))

(defun make-reply (replies)
  (let ((rng (make-random (length replies))))
    #'(lambda ()
        (format t "~A~2%" (aref replies (random rng)))) ))

(setf (symbol-function 'right-reply)
      (make-reply #("Right!" "OK. That's good." "Just what I would have said!" "Close enough. (In fact, exactly right.)" "Great! Super! Let's keep going..." "Of course! (Why didn't I think of that?)" "Yep. Nice work.")))

(setf (symbol-function 'wrong-reply)
      (make-reply #("In a word: no." "Not quite right. One more time." "Try again. You can get it right.")))
