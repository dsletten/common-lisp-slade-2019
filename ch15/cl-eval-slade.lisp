;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               cl-eval-slade.lisp
;;;;
;;;;   Started:            Tue Apr 14 20:11:02 2020
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

(defpackage :cl-eval-slade (:use :common-lisp))

(in-package :cl-eval-slade)

(defun cl-eval (exp env)
  (cond ((or (numberp exp) (functionp exp) (characterp exp) (stringp exp)) exp)
        ((symbolp exp) (value exp env))
        ((consp exp)
         (case (car exp)
           (quote (cadr exp))
           (if (cl-eval-if exp env))
           (progn (cl-eval-progn exp env))
           (lambda (cl-eval-lambda exp env))
           (defun (cl-eval-defun exp env))
           (setq (cl-eval-setq exp env))
           (case (cl-eval-case exp env))
           (cond (cl-eval-cond exp env))
           (and (cl-eval-and exp env))
           (or (cl-eval-or exp env))
           (let (cl-eval-let exp env))
           (otherwise (cl-eval-call exp env)))) ))

;;;
;;;    Locate function definition:
;;;    1. Symbol - find in ENV or builtin, i.e., COMMON-LISP package.
;;;    2. LAMBDA expression - wrap in FUNCTION object that adds new bindings based on LAMBDA-list params.
;;;    
(defun cl-eval-call (exp env)
  (let* ((func (cl-eval (car exp) env))
         (func2 (if (and (consp func) (eq 'lambda (car func)))
                    (cl-eval-lambda func env)
                    func)))
    (apply func2 (mapcar #'(lambda (arg) (cl-eval arg env)) (cdr exp)))) )

(defun cl-eval-if (exp env)
  (if (cl-eval (cadr exp) env)
      (cl-eval (caddr exp) env)
      (cl-eval (cadddr exp) env)))

;;;
;;;    This should not call CL-EVAL-SEQUENCE if there are no sub forms!
;;;    (cl-eval '(progn))
;;;    
(defun cl-eval-progn (exp env)
  (cl-eval-sequence (cdr exp) env)) ; ???

;;;
;;;    This is only called by CL-EVAL-PROGN and indirectly by CL-EVAL-LAMBDA to evaluate the body (That function returns a function that calls CL-EVAL-SEQUENCE.)
;;;    In other words, this is really just a form of PROGN.
;;;    The function definition is odd. Slade pointedly draws attention to the fact that the base case is not (NULL EXPS) because this
;;;    allows the definition to be "properly tail recursive"???? WTF is he talking about?...
;;;    I guess he's trying to ensure that the final call returns the value of its form.
;;;    
;;;    My version in simple-eval.lisp is properly tail recursive... (My version in cl-eval.lisp is iterative!)
;;;    
(defun cl-eval-sequence (exps env) ; ???
  (cond ((null (cdr exps)) (cl-eval (car exps) env))
        (t (cl-eval (car exps) env)
           (cl-eval-sequence (cdr exps) env))))

;;;
;;;    Not quite right...
;;;   
;; (defun cl-eval-sequence (exps env)
;;   (do ((exps exps (rest exps))
;;        (result nil (cl-eval (first exps))))
;;       ((endp exps) result)))

(defclass environment ()
  ((id :initarg :id :reader id)
   (contents :initarg :contents :accessor contents)
   (next :initarg :next :accessor next))
  (:documentation "Class representing a binding and a link to the next binding."))

(defun make-environment (id val env localp)
  (let ((new (make-instance 'environment :id id :contents val :next nil)))
    (cond ((and env (not localp))
           (setf (next new) (next env))
           (setf (next env) new))
          ((and env localp)
           (setf (next new) env)))
    new))

;; (defun make-environment (id val env localp)
;;   (let ((new (make-instance 'environment :id id :contents val :next nil)))
;;     (when env
;;       (if localp
;;           (setf (next new) env)
;;           (setf (next new) (next env)
;;                 (setf (next env) new))))
;;     new))

(defmethod print-object ((env environment) str)
;(defmethod print-object ((env environment) (str stream))
  (format str "~%~A:~10T~A" (id env) (contents env))
  (when (next env)
    (print-object (next env) str)))

;(defgeneric cl-env-lookup...
;;;
;;;    Try to find an environment (binding) containing ID.
;;;    If current environment does not hold ID, check the NEXT link. Otherwise NIL.
;;;
;;;    In case of SETQ/DEFUN (via SET-VALUE) create a new (global) environment if one does not exist.
;;;    
(defmethod cl-env-lookup ((env environment) id localp createp)
  (cond ((eq (id env) id) env)
        ((next env)
         (cl-env-lookup (next env) id localp createp))
        (createp
         (make-environment id nil env localp))
        (t nil)))

(defmethod cl-env-lookup (env id localp createp)
  (declare (ignore env localp createp)) ; !!!!!!!!!!!!!!!!
  id)

;(defgeneric contents...
;;;
;;;    Only called by VALUE when ID (a symbol) not found in environment.
;;;    Otherwise CONTENTS method of ENVIRONMENT object called...
;;;    
(defmethod contents (symbol)
  (cond ((fboundp symbol) (symbol-function symbol))
        ((functionp symbol) symbol) ; Can't reach here...
        ((boundp symbol) (symbol-value symbol))
        (t symbol)))

;; (defmethod contents (obj)
;;   (cond ((fboundp obj) (symbol-function obj))
;;         ((functionp obj) obj) ; This clause is pointless. If OBJ were a function, it would have caused an error in the previous clause (FBOUNDP)! Only way we can reach here is if OBJ is _not_ a function object!
;;         ((boundp obj) (symbol-value obj))
;;         (t obj)))

;;;
;;;    Create new environment for LAMBDA expression, binding its params to the args it received.
;;;    
(defun bind-variables (ids values env)
  (cond ((consp ids)
         (bind-variable (car ids) (car values) (bind-variables (cdr ids) (cdr values) env)))
        ((null ids) env)
        (t (bind-variable ids values env))))

;;;
;;;    Only local bindings??
;;;    
;;;    CL-EVAL-LAMBDA -> BIND-VARIABLES -> BIND-VARIABLE -> MAKE-ENVIRONMENT (Always local bindings for execution of LAMBDA expression)
;;;
(defun bind-variable (identifier value env)
  (make-environment identifier value env t))

;;;
;;;    Try to find symbol ID in ENV. Otherwise look for built-in value (COMMON-LISP package) as function or variable.
;;;    
(defun value (id env)
  (let ((result (cl-env-lookup env id nil nil)))
    (cond (result (contents result))
          (t (contents id)))) )

;;;
;;;    CL-EVAL-SETQ/CL-EVAL-DEFUN -> SET-VALUE -> CL-ENV-LOOKUP (Possibly -> MAKE-ENVIRONMENT Always LOCALP NIL -- Never local binding)
;;;    
(defun set-value (id env val localp)
  (setf (contents (cl-env-lookup env id localp t)) val))

(defun cl-eval-lambda (exp env)
;; (print `(cl-eval-sequence ,(cddr exp)
;;                           (bind-variables ,(cadr exp) args ,env)))

  #'(lambda (&rest args)
      (cl-eval-sequence (cddr exp)
                        (bind-variables (cadr exp) args env))))

(defun cl-eval-setq (exp env)
  (let ((place (cadr exp)))
    (set-value place env (cl-eval (caddr exp) env) nil)))

;; (defun cl-eval-defun (exp env)
;;   (let ((name (cadr exp)))
;;     (set-value name env `(lambda ,(caddr exp) ,@(cdddr exp)) nil)))

(defun cl-eval-defun (exp env)
  (destructuring-bind (name lambda-list . body) (rest exp)
    (set-value name env `(lambda ,lambda-list ,@body) nil)))

;; (defun cl-eval-cond
;; (defun cl-eval-case
;; (defun cl-eval-or
;; (defun cl-eval-and
;; (defun cl-eval-let
    
