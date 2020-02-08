;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch15.lisp
;;;;
;;;;   Started:            Sat Feb  1 17:13:58 2020
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

(defpackage :ch15 (:use :common-lisp :test))

(in-package :ch15)

;;;
;;;    15.7.1
;;;    This isn't quite what the exercise specifies...CLEAN-SLATE is a macro, but it defines a _function_ DIRTY-SLATE.
;;;    Ha! Slade defines DIRTY-SLATE as a function too!!
;;;    
(defmacro clean-slate ()
  (let* ((package (gensym))
	 (package-name (symbol-name package))
	 (current-package *package*))
    `(progn
       (defpackage ,package (:use :common-lisp) (:intern :dirty-slate))
       (in-package ,package)
       (let ((f (find-symbol "DIRTY-SLATE")))
	 (setf (symbol-function f)
	       #'(lambda () 
		   (in-package ,(package-name current-package))
		   (delete-package ,package-name)))) )))
;       (defmacro dirty-slate () `(list 'foo 'bar)))))
;       (defmacro ,(intern "DIRTY-SLATE" `',package) () `(list 'foo 'bar)))))
	 ;; `(progn
	 ;;    (in-package ,,current-package)
	 ;;    (delete-package ,,package)))) ))

