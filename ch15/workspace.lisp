;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               workspace.lisp
;;;;
;;;;   Started:            Sun Feb  2 20:59:05 2020
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

(defpackage :workspace (:use :common-lisp :test))

(in-package :workspace)

(defconstant ws-prompt "WS> ")

(defun make-workspace (filename)
  (block main
    (format t "Starting workspace. End with :save-ws ~%")
    (with-open-file (ws-stream filename
			       :direction :output
			       :if-exists :supersede)
      (cond ((null ws-stream)
	     (format *error-output* "Error: Could not open workspace file!~%")
	     (return-from make-workspace))
	    (t (format ws-stream ";; workspace file ~%")
	       (unwind-protect
		 (let ((*read-eval* nil))
		   (loop
		    (format t "~&~A" ws-prompt)
		    (force-output)
		    (let ((input (read)))
		      (cond ((eq input :save-ws) (return-from main))
			    ((listp input) (format ws-stream "~A~%" input)))
		      (print (eval input)))) )))) ))
  (format t "Workspace saved in file: ~A~%" filename))

