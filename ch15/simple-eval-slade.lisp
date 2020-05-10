;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               simple-eval-slade.lisp
;;;;
;;;;   Started:            Sun Mar 15 02:13:15 2020
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
;(load "/home/slytobias/lisp/packages/test.lisp")
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :simple-eval-slade (:use :common-lisp :test))

(in-package :simple-eval-slade)

(defun simple-eval (exp &optional env)
  (cond ((numberp exp) exp) ;    Self-evaluating
        ((stringp exp) exp) ;    .
        ((characterp exp) exp) ; .
        ((atom exp) (simple-env-lookup exp env)) ; Lisp-1
        ((eq (car exp) 'quote) (cadr exp))                       ; Special operator (macros treated as such)
        ((eq (car exp) 'cond) (simple-eval-cond (cdr exp) env))  ; .
        (t (simple-apply (car exp) (simple-eval-list (cdr exp) env) env)))) ; Arbitrary function

(defun simple-env-lookup (id env)
  (cdr (assoc id env)))

;;;
;;;    Evaluate list of test clauses.
;;;    Must include final T clause!!
;;;    
(defun simple-eval-cond (exp env)
  (cond ((simple-eval (caar exp) env) (simple-eval (cadar exp) env))
        (t (simple-eval-cond (cdr exp) env))))

;;;
;;;    Evaluate all args to a function form before applying the function.
;;;    
(defun simple-eval-list (exp env)
  (cond ((null exp) nil)
        (t (cons (simple-eval (car exp) env)
                 (simple-eval-list (cdr exp) env)))) )

(defun simple-apply (proc args env)
  (cond ((atom proc)
         (case proc
           (car (caar args))
           (cdr (cdar args))
           (cons (cons (car args) (cadr args)))
           (atom (atom (car args)))
           (eq (eq (car args) (cadr args)))
           (otherwise (simple-apply (simple-eval proc env) args env))))
        ((eq (car proc) 'lambda)
         (simple-eval (caddr proc)
                      (simple-pairlis (cadr proc) args env)))
        ((eq (car proc) 'defun)
         (simple-apply (caddr proc) args (cons (cons (cadr proc) (caddr proc)) env)))) ) ; Ad hoc add mapping to environment!!: name -> lambda expression

;;;
;;;    Augment an enviroment (via shadowing) by adding new bindings.
;;;    
(defun simple-pairlis (x y env)
  (cond ((null x) env)
        (t (cons (cons (car x) (car y))
                 (simple-pairlis (cdr x) (cdr y) env)))) )
