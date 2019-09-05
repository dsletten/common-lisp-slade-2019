;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               compare-files.lisp
;;;;
;;;;   Started:            Mon Aug 12 02:21:50 2019
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

(defpackage :compare-files (:use :common-lisp :test))

(in-package :compare-files)

(defun compare-files (file1 file2)
  (with-open-file (stream1 file1 :if-does-not-exist :error)
    (with-open-file (stream2 file2 :if-does-not-exist :error)
      (compare-streams stream1 stream2 "File"))))

(defun compare-streams (stream1 stream2 &optional (label "Stream"))
  (let ((line1 (read-line stream1 nil nil))
        (line2 (read-line stream2 nil nil)))
    (cond ((null line1) (or (null line2) (error "~A 2 is longer." label)))
          ((null line2) (error "~A 1 is longer." label))
          ((string= line1 line2) (compare-streams stream1 stream2 label))
          (t (format t "1: ~S~%" line1)
             (format t "2: ~S~2%" line2)
             (compare-streams stream1 stream2 label)))) )
