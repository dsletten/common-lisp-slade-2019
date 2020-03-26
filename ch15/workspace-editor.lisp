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
;;;;   CAR/CDR/FIND/TOP/UP move CONTEXT
;;;;   Taken literally, executing TOP should invalidate the HISTORY. There is no further UP to go to once you're at the TOP already!
;;;;   The UP operation could instead be viewed as BACK...
;;;;
;;;;   SUBST may drastically alter tree structure -> CONTEXT/HISTORY reset.
;;;;   Example:
;;;;       Consider the expression (((a b) (c d)) (e (f g)))
;;;;       Suppose the user has navigated down to the CONS (F G) as current CONTEXT (CADADR).
;;;;       The HISTORY would be:  V-- CDADR V-- CADR  V-- CDR     V-- Top
;;;;                             (((F G))   (E (F G)) ((E (F G))) (((A B) (C D)) (E (F G))))
;;;;       Then if the user performs a GLOBAL-SUBSTITUTE on (E (F G)) with another CONS, say (X Y),
;;;;       then the current CONTEXT is not even part of the expression anymore: (((A B) (C D)) (X Y))
;;;;       Furthermore, the history is meaningless until you go three levels up.
;;;;       Even then it has changed: V-- CDADR* V-- CADR* V-- CDR V-- Top             (* Invalid)
;;;;                                (((F G))    (E (F G)) ((X Y)) (((A B) (C D)) (X Y)))
;;;;       If the user executes REPLACE-CAR, say with Z, after the GLOBAL-SUBSTITUTE then the CONTEXT undergoes
;;;;       a meaningless modification: (Z G)
;;;;       This destructively modifies the invalid parts of the HISTORY: (((Z G)) (E (Z G)) ((X Y)) (((A B) (C D)) (X Y)))
;;;;       But with no impact on the expression itself: (((A B) (C D)) (X Y))
;;;;       Consequently, executing GLOBAL-SUBSTITUTE should simply reset CONTEXT and clear HISTORY.
;;;;       
;;;;   DEL/INS/REP modify tree
;;;;
(load "/home/slytobias/lisp/packages/surgery.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :workspace-editor (:use :common-lisp :test :surgery))

(in-package :workspace-editor)

(defvar *commands* '((("help" "h" "?") "print help information")
                     (("car" "<") "move context down by car")
                     (("cdr" ">") "move context down by cdr")
                     (("del" "d") "delete car of current context")
                     (("exit" "e") "save current definition and exit")
                     ("(<EXPR>)" "evaluate given <EXPR>")
                     (("find" "f") "move context to matching expression")
                     (("ins" "i") "insert at current context")
                     (("print" "p") "print current context")
                     ("pd" "print definition")
                     (("quit" "q") "exit editor without saving")
                     (("rep" "r") "replace car of current context")
                     (("save" "s") "save current definition")
                     ("set" "set current definition")
                     ("subst" "global substitution in definition")
                     (("top" "t") "move context to top definition")
                     (("up" "u") "move context up")))

(defun print-menu ()
  (loop for (command description) in *commands*
        do (print-command command description)))

(defconstant menu-spacing 14)

(defun print-command (command description)
  (if (listp command)
      (format t "~V@<~{~}~>" menu-spacing "~A~^ | " command)
      (format t "~V@<~A~>" menu-spacing command))
  (format t "~A~%" description))

(defun prompt-read (p)
  (write-string p)
  (force-output)
  (let ((*read-eval* nil))
    (read)))

(defun read-command ()
  (prompt-read "WED> "))

(let ((ws-table (make-hash-table :test #'eq))
      (ws-definitions '()))

; Debugging only
(defun lookup (symbol) 
  (gethash symbol ws-table))
(defun definitions () ws-definitions)

  (defun wed (symbol)
    (multiple-value-bind (definition presentp) (gethash symbol ws-table)
      (if presentp
          (format t "Modifying definition of: ~S~%" symbol)
          (format t "Creating definition of: ~S~%" symbol))
      (let ((expression (make-active-expression definition)))
        (catch 'main
          (loop
           (process-command (read-command) symbol expression)))
        (format t "Exiting editor.~%"))))

  (defun save-definition (symbol expression)
    (let ((definition (definition expression)))
      (pushnew symbol ws-definitions)
      (setf (gethash symbol ws-table) definition)
      (eval definition)))
  
  (defun save-ws (filename)
    (with-open-file (ws-stream filename
                               :direction :output
                               :if-exists :supersede)
      (cond ((null ws-stream) (format *error-output* "Error: Could not open workspace file!~%"))
            (t (format ws-stream ";; workspace file ~%")
               (unwind-protect
                 (dolist (symbol ws-definitions)
                   (format ws-stream ";; WED definition: ~S~2%" symbol)
                   (format ws-stream "~S~%" (gethash symbol ws-table)))) )))) )

(defun process-command (command symbol expression)
  (case command
    ((help h ?) (print-menu))
    ((car <) (setf (context expression) (car (context expression))))
    ((cdr >) (setf (context expression) (cdr (context expression))))
    ((del d) (delete-car expression))
    ((exit e) (save-definition symbol expression) (throw 'main nil))
    ((find f) (find-subexpression expression))
    ((ins i) (insert-subexpression expression))
    ((print p) (format t "~S~%" (context expression)))
    (pd (format t "~S~%" (definition expression)))
    ((rep r) (replace-car expression))
    ((save s) (save-definition symbol expression))
    (set (set-definition expression))
    (subst (global-substitute expression))
    ((quit q) (throw 'main nil))
    ((t top) (setf (context expression) (definition expression)) (clear-history expression))
    ((up u) (up-context expression))
    (otherwise (when (consp command) (format t "~S~%" (eval command)))) ))

;;;
;;;    Debugging
;;;    
;; (defun carf (expression) (setf (context expression) (car (context expression))))
;; (defun cdrf (expression) (setf (context expression) (cdr (context expression))))

(defun set-definition (expression)
  (let ((new-definition (prompt-read "Definition: ")))
    (setf (definition expression) new-definition)))

(defun global-substitute (expression)
  (let ((old (prompt-read "Old Expression: "))
	(new (prompt-read "New Expression: ")))
    (setf (definition expression) (nsubst new old (definition expression) :test #'equal)))) ; Need to call setter

(defun find-subexpression (expression)
  (let ((new-context (tree-find (prompt-read "Expression: ") (definition expression) :test #'equal)))
    (if (null new-context)
	(format t "Could not find sub-expression.~%")
        (setf (context expression) new-context))))

(defun insert-subexpression (expression)
  (with-slots (definition context) expression
    (let* ((sub (prompt-read "Expression: "))
	   (new-expression (ntree-splice-before context sub)))) ))

(defun delete-car (expression) ; This may not work for top-level CONS...See below
  (with-slots (definition context) expression
    (format t "Deleting: ~S~%" (car context))
    (ntree-snip context)))

(defun replace-car (expression)
  (setf (car (context expression)) (prompt-read "Expression: ")))

(defun up-context (expression)
  (let ((old-history (history expression)))
    (if (null old-history)
        (format t "No further context.~%")
        (with-slots (context history) expression
          (setf context (pop old-history)
                history old-history)))) )

;;;
;;;    The ACTIVE-EXPRESSION wraps a definition to keep track of where in the tree the
;;;    CONTEXT is currently pointing. The HISTORY preserves previous context points, although
;;;    this is not necessarily complete as a FIND operation may jump deep into the tree.
;;;    
(defclass active-expression ()
  ((definition :accessor definition :initarg :definition)
   (context :accessor context :initform nil)
   (history :accessor history :initform '())))

;;;
;;;    Whenever the DEFINITION is set there is not point in preserving previous history.
;;;    CONTEXT is reset too.
;;;    
(defmethod (setf definition) :after (location (expr active-expression))
  (setf (slot-value expr 'context) location)
  (clear-history expr))

;;;
;;;    Directly setting the CONTEXT causes current context to be saved in HISTORY.
;;;    
(defmethod (setf context) :before (location (expr active-expression))
  (declare (ignore location))
  (push (context expr) (history expr)))

(defun make-active-expression (definition)
  (make-instance 'active-expression :definition definition))

(defmethod initialize-instance :after ((expr active-expression) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value expr 'context) (definition expr)))

(defun clear-history (expression)
  (setf (slot-value expression 'history) '()))
