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
;;;;   Destructive functions screw up history???
;;;;
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :workspace-editor (:use :common-lisp :test))

(in-package :workspace-editor)

(defvar *commands* '(("?" "print help information")
		     (("car" "<") "move context down by car")
		     (("cdr" ">") "move context down by cdr")
		     ("del" "delete car of current context")
		     ("exit" "save current definition and exit")
		     ("(<EXPRESSION>)" "evaluate given <EXPRESSION>")
		     ("find" "move context to matching expression")
		     (("ins" "i") "insert at current context")
		     ("p" "print current context")
		     ("pd" "print definition")
		     ("quit" "exit editor without saving")
		     ("rep" "replace car of current context")
		     ("save" "save current definition")
		     ("set" "set current definition")
		     ("subst" "global substitution in definition")
		     (("top" "t") "move context to top definition")
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

(defun prompt-read (p)
  (format t p)
  (force-output)
  (let ((*read-eval* nil))
    (read)))

(defun read-command ()
  (prompt-read "WED> "))

(let ((ws-table (make-hash-table :test #'eq)))
  (defun wed (symbol)
    (multiple-value-bind (definition presentp) (gethash symbol ws-table)
      (if presentp
	  (format t "Modifying definition of: ~A~%" symbol)
	  (format t "Creating definition of: ~A~%" symbol))
      (let ((expression (make-active-expression definition)))
	(catch 'main
	  (loop
	   (process-command (read-command) symbol expression)))
	(format t "Exiting editor.~%"))))

  (defun process-command (command symbol expression)
    (case command
      (? (print-menu))
      ((car <) (setf (context expression) (car (context expression))))
      ((cdr >) (setf (context expression) (cdr (context expression))))
      (del (delete-car expression))
      (exit (save-definition symbol expression) (throw 'main nil))
      (find (find-subexpression expression))
      ((i ins) (insert-subexpression expression))
      (p (format t "~A~%" (context expression)))
      (pd (format t "~A~%" (definition expression)))
      (rep (replace-car expression))
      (save (save-definition symbol expression))
      (set (set-definition expression))
      (subst (global-substitute expression))
      (quit (throw 'main nil))
      ((t top) (setf (context expression) (definition expression)))
      (u (up-context expression))
      (otherwise (when (consp command) (format t "~A~%" (eval command)))) ))
  
  ;; (defun set-definition (symbol expression)
  ;;   (format t "Definition: ")
  ;;   (force-output)
  ;;   (let ((new-definition (read)))
  ;;     (stash-definition symbol expression new-definition)))
  
  ;; (defun stash-definition (symbol expression new-definition)
  ;;   (setf (gethash symbol ws-table) new-definition)
  ;;   (eval new-definition)
  ;;   (with-slots (definition context) expression
  ;;     (setf definition new-definition
  ;; 	    context new-definition)))
  
  ;; (defun global-substitute (symbol expression)
  ;;   (format t "Old Expression: ")
  ;;   (force-output)
  ;;   (let ((old (read)))
  ;;     (format t "New Expression: ")
  ;;     (force-output)
  ;;     (let ((new (read)))
  ;; 	(stash-definition symbol expression (subst new old (definition expression)))) ))
  
  ;; (defun replace-car (symbol expression)
  ;;   (format t "Expression: ")
  ;;   (force-output)
  ;;   (let ((new-car (read)))
  ;;     (setf (car (context expression)) new-car)
  ;;     (stash-definition symbol expression (definition expression)))) )

  (defun set-definition (expression)
    (let ((new-definition (prompt-read "Definition: ")))
      (with-slots (definition context) expression
	(setf definition new-definition
	      context new-definition)))) ; Clear history?!
  
  (defun save-definition (symbol expression)
    (let ((definition (definition expression)))
      (pushnew symbol *ws-definitions*)
      (setf (gethash symbol ws-table) definition)
      (eval definition)))
  
  (defun global-substitute (expression)
    (let ((old (prompt-read "Old Expression: "))
	  (new (prompt-read "New Expression: ")))
      (with-slots (definition) expression
	(setf definition (subst new old definition)))) )
  
  (defun find-subexpression (expression)
    (labels ((find-it (sub expression)
	       (cond ((atom expression) nil)
		     ((eql (car expression) sub) expression)
		     (t (or (find-it sub (car expression))
			    (find-it sub (cdr expression)))) )))
      (let ((new-context (find-it (prompt-read "Expression: ") (definition expression))))
	(if (null new-context)
	    (format t "Could not find sub-expression.~%")
	    (setf (context expression) new-context)))) )

  ;; (defun insert-subexpression (expression)
  ;;   (let ((sub (prompt-read "Expression: ")))
  ;;     (push sub (context expression))))

  (defun insert-subexpression (expression)
    (labels ((rebuild (expression context sub)
	       (cond ((atom expression) expression)
		     ((eq expression context) (cons sub context))
		     (t (cons (rebuild (car expression) context sub)
			      (rebuild (cdr expression) context sub)))) ))
      (with-slots (definition context) expression
	(let* ((sub (prompt-read "Expression: "))
	       (new-expression (rebuild definition context sub)))
	  (setf definition new-expression)
	  (push sub context)))) )

  (defun delete-car (expression) ; This may not work for top-level CONS...See below
    (labels ((rebuild (expression context)
	       (cond ((atom expression) expression)
		     ((eq expression context) (cdr context))
		     (t (cons (rebuild (car expression) context)
			      (rebuild (cdr expression) context)))) ))
      (with-slots (definition context) expression
	(format t "Deleting: ~A~%" (car context))
	(let ((new-expression (rebuild definition context)))
	  (setf definition new-expression
		context (cdr context)))) )) ; CONSP?

  (defun replace-car (expression)
    (setf (car (context expression)) (prompt-read "Expression: ")))
  
  (defun save-ws (filename)
    (with-open-file (ws-stream filename
			       :direction :output
			       :if-exists :supersede)
      (cond ((null ws-stream)
	     (format *error-output* "Error: Could not open workspace file!~%")
	     (return-from save-ws))
	    (t (format ws-stream ";; workspace file ~%")
	       (unwind-protect
		 (dolist (symbol *ws-definitions*)
		   (format ws-stream ";; WED definition: ~A~2%" symbol)
		   (format ws-stream "~A~%" (gethash symbol ws-table)))) )))) )

;      (stash-definition symbol expression (definition expression)))) )

(defun up-context (expression)
  (let ((old-history (history expression)))
    (if (null old-history)
	(format t "No further context.~%")
;        (setf (context expression) (pop history)))) )  ; Have to avoid :BEFORE method!!
        (with-slots (context history) expression
	  (setf context (pop old-history)
		history old-history)))) )

(defclass active-expression ()
  ((definition :reader definition :initarg :definition)
   (context :accessor context :initform nil)
   (history :accessor history :initform '())))

(defmethod (setf context) :before (location (expr active-expression))
  (push (context expr) (history expr)))
;(print (history expr)))
;  (setf (previous expr) (context expr))) ; Should be stack???

(defun make-active-expression (definition)
  (make-instance 'active-expression :definition definition))

(defmethod initialize-instance :after ((expr active-expression) &rest initargs)
  (declare (ignore initargs))
  (setf (context expr) (definition expr)) ; Makes HISTORY (())???
  (pop (history expr)))

;; Flaky delete...
;; WORKSPACE-EDITOR(149): (wed 'fact)
;; Creating definition of: FACT
;; WED> set
;; Definition: (defun fact (n) (cond ((zero n) 0) (t (+ n (fact (- nn 1))))))
;; WED> >
;; WED> >
;; WED> p
;; ((N) (COND ((ZERO N) 0) (T (+ N (FACT (- NN 1))))))
;; WED> t
;; WED> p
;; (DEFUN FACT (N) (COND ((ZERO N) 0) (T (+ N (FACT (- NN 1))))))
;; WED> i
;; Expression: :foo
;; WED> p
;; (FOO DEFUN FACT (N) (COND ((ZERO N) 0) (T (+ N (FACT (- NN 1))))))
;; WED> del
;; Deleting: FOO
;; WED> p
;; (DEFUN FACT (N) (COND ((ZERO N) 0) (T (+ N (FACT (- NN 1))))))
;; WED> >
;; WED> >
;; WED> >
;; WED> p
;; ((COND ((ZERO N) 0) (T (+ N (FACT (- NN 1))))))
;; WED> t
;; WED> p
;; (FOO DEFUN FACT (N) (COND ((ZERO N) 0) (T (+ N (FACT (- NN 1))))))
;; WED> del
;; Deleting: FOO
;; WED> p
;; (DEFUN FACT (N) (COND ((ZERO N) 0) (T (+ N (FACT (- NN 1))))))
;; WED> t
;; WED> p
;; (DEFUN FACT (N) (COND ((ZERO N) 0) (T (+ N (FACT (- NN 1))))))
;; WED> >
;; WED> >
;; WED> >
;; WED> p
;; ((COND ((ZERO N) 0) (T (+ N (FACT (- NN 1))))))
;; WED> t
;; WED> p
;; (DEFUN FACT (N) (COND ((ZERO N) 0) (T (+ N (FACT (- NN 1))))))
