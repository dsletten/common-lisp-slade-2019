;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               workspace-editor.lisp
;;;;
;;;;   Started:            Mon Feb  3 23:05:35 2020
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

(defpackage :workspace-editor (:use :common-lisp :test))

(in-package :workspace-editor)

(defvar *commands* '(("?" "print help information")
		     (("car" "<") "move context down by car")
		     (("cdr" ">") "move context down by cdr")
		     ("del" "delete car of current context")
		     ("exit" "save current definition and exit")
		     ("(exp)" "evaluate given expression")
		     ("find" "move context to matching expression")
		     ("i" "insert at current context")
		     ("p" "print current context")
		     ("pd" "print definition")
		     ("quit" "exit editor without saving")
		     ("rep" "replace car of current context")
		     ("save" "save current definition")
		     ("set" "set current definition")
		     ("subst" "global substitution in definition")
		     ("top" "move context to top definition")
		     ("u" "move context up")))

(defun print-menu ()
  (loop for (command description) in *commands*
	do (print-command command description)))

(defconstant menu-spacing 8)

(defun print-command (command description)
  (if (listp command)
      (format t "~V@<~{~}~>" menu-spacing "~A~^ | " command)
      (format t "~V@<~A~>" menu-spacing command))
  (format t "  ~A~%" description))

(defvar *ws-definitions* '())

(defun prompt ()
  (format t "WED> ")
  (force-output))

(let ((ws-table (make-hash-table :test #'eq)))
  (defun wed (symbol)
    (multiple-value-bind (definition presentp) (gethash symbol ws-table)
      (if presentp
	  (format t "Modifying definition of: ~A~%" symbol)
	  (format t "Creating definition of: ~A~%" symbol))
      (let ((expression (make-active-expression definition)))
	(catch 'main
	  (loop
	   (prompt)
	   (let* ((*read-eval* nil)
		  (command (read)))
	     (process-command command symbol expression)))) )))

  (defun process-command (command symbol expression)
    (case command
      (? (print-menu))
      ((car <) (setf (context expression) (car (context expression))))
      ((cdr >) (setf (context expression) (cdr (context expression))))
      (p (format t "~A~%" (context expression)))
      (pd (format t "~A~%" (definition expression)))
      (rep (replace-car symbol expression))
      (set (set-definition symbol expression))
      (subst (global-substitute symbol expression))
      (quit (throw 'main nil))
      (top (setf (context expression) (definition expression)))
      (u (setf (context expression) (previous expression)))
      (otherwise (format t "~A~%" (eval command)))) )
  
  (defun set-definition (symbol expression)
    (format t "Definition: ")
    (force-output)
    (let ((new-definition (read)))
      (stash-definition symbol expression new-definition)))
  
  (defun stash-definition (symbol expression new-definition)
    (setf (gethash symbol ws-table) new-definition)
    (eval new-definition)
    (with-slots (definition context) expression
      (setf definition new-definition
	    context new-definition)))
  
  (defun global-substitute (symbol expression)
    (format t "Old Expression: ")
    (force-output)
    (let ((old (read)))
      (format t "New Expression: ")
      (force-output)
      (let ((new (read)))
	(stash-definition symbol expression (subst new old (definition expression)))) ))
  
  (defun replace-car (symbol expression)
    (format t "Expression: ")
    (force-output)
    (let ((new-car (read)))
      (setf (car (context expression)) new-car)
      (stash-definition symbol expression (definition expression)))) )

(defclass active-expression ()
  ((definition :reader definition :initarg :definition)
   (context :accessor context :initform nil)
   (previous :accessor previous :initform nil)))

(defmethod (setf context) :before (location (expr active-expression))
  (setf (previous expr) (context expr))) ; Should be stack???

(defun make-active-expression (definition)
  (make-instance 'active-expression :definition definition))

(defmethod initialize-instance :after ((expr active-expression) &rest initargs)
  (declare (ignore initargs))
  (setf (context expr) (definition expr)))
