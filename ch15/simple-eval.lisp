;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               simple-eval.lisp
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
;;;;   Notes: This only defines a Lisp-1 language. Binding of a symbol is looked up in environment
;;;;   without regard for variable/function namespace.
;;;;
;;;;   https://en.wikipedia.org/wiki/Mathematical_operators_and_symbols_in_Unicode
;;;;
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :simple-eval (:use :common-lisp :test))

(in-package :simple-eval)

(defconstant addition-identity 0)
(defconstant multiplication-identity 1)

;;;
;;;    ENV is dotted-pair alist of "bindings". See TEST-SIMPLE-EVAL below.
;;;    CLHS 3.1.2.1 Form Evaluation
;;;    Forms fall into three categories: symbols, conses, and self-evaluating objects.
;;;    
(defun simple-eval (exp &optional env)
  (cond ((consp exp)
         (cond ((simple-special-operator-p (first exp)) ; Consider macros as special operators...
                (special-eval exp env))
               ;; ((lambda-expression-p exp)
               ;;       (simple-lambda exp env))
               (t (simple-apply (first exp) (simple-eval-list (rest exp) env) env)))) ; Arbitrary function
        ((constantp exp) exp) ; Self-evaluating
        ((atom exp) (simple-env-lookup exp env)) ; Symbols???
        (t (error "How did we get here?!?"))))

(defun simple-special-operator-p (obj)
  (case obj
    ((quote cond if and or) t)
    (otherwise nil)))

(defun special-eval (exp env)
  (ecase (first exp)
    (quote (if (null (rest (rest exp))) (second exp) (error "Wrong number of arguments for QUOTE.")))
    (cond (simple-eval-cond (rest exp) env))
    (if (simple-eval-if (rest exp) env))
    (and)
    (or)))

(defun simple-env-lookup (id env)
  (cdr (assoc id env)))

;;;
;;;    Evaluate list of test clauses.
;;;    Must include final T clause!! <-- Nope
;;;    
(defun simple-eval-cond (body env)
  (cond ((null body) nil)
        (t (destructuring-bind ((test . clauses) . more) body
             (if (simple-eval test env) 
                 (do ((clauses (rest clauses) (rest clauses))
                      (result (simple-eval (first clauses)) (simple-eval (first clauses))))
                     ((endp clauses) result))
;                (dolist (clause clauses)
;                  (simple-eval clause env))
;                (simple-eval-list clauses env)
;                (progn (simple-eval clause env) (simple-eval-
                 (simple-eval-cond more env)))) ))

(defun simple-eval-if (body env)
  (destructuring-bind (test then &optional else) body
    (if (simple-eval test env)
        (simple-eval then env)
        (simple-eval else env))))

(defun simple-eval-list (exp env)
  (cond ((null exp) nil)
        (t (cons (simple-eval (car exp) env)
                 (simple-eval-list (cdr exp) env)))) )

;;;
;;;    Only LAMBDA really needs ENV here?! It may add new bindings to the environment in which its body is evaluated.
;;;    
(defun simple-apply (proc args env)
  (cond ((null proc) (error "Whaaa?"))
        ((atom proc)
         (case proc
           (car (expected-args 1 args)
                (car (first args)))
           (cdr (expected-args 1 args)
                (cdr (first args)))
           (cons (expected-args 2 args)
                 (cons (first args) (second args)))
           (atom (expected-args 1 args)
                 (atom (first args)))
           (not (expected-args 1 args)
                (not (first args)))
           (eq (expected-args 2 args)
               (eq (first args) (second args)))
           (eql (expected-args 2 args)
                (eql (first args) (second args)))
           (equal (expected-args 2 args)
                  (equal (first args) (second args)))
           (equalp (expected-args 2 args)
                   (equalp (first args) (second args)))
           (print (expected-args 1 args)
                  (print (first args)))
           (= (expected-args 1 args :count :at-least) ; Refactor. Same structure: = < > <= >= and + * and / -
	      (relational #'= args))
           ;; (/= (expected-args 1 args :count :at-least)
           ;;     (destructuring-bind (x &optional (y nil y-supplied-p) &rest more) args
           ;;    (if y-supplied-p
           ;;        (and (/= x y) 
           ;;             (or (null more)
           ;;                 (simple-apply proc (cons y more) env)))
           ;;        t)))
           (/= (expected-args 1 args :count :at-least) ; This one is tricky: With n args -> (n 2) pairwise comparisons!!
               (destructuring-bind (x &optional (y nil y-supplied-p) &rest more) args
                 (if y-supplied-p
                     (if (null more)
                         (/= x y) 
                         (every #'identity (simple-eval-list (mapcon #'(lambda (l) (mapcar #'(lambda (elt) (list proc (first l) elt)) (rest l))) args) env))) ; Don't really need ENV. All args eval'd already.
                     t)))
           (< (expected-args 1 args :count :at-least)
	      (relational #'< args))
           (<= (expected-args 1 args :count :at-least)
	       (relational #'<= args))
           (> (expected-args 1 args :count :at-least)
	      (relational #'> args))
           (>= (expected-args 1 args :count :at-least)
	       (relational #'>= args))
           (+ (expected-args 0 args :count :at-least)
	      (arithmetic-0 #'+ args addition-identity))
           (- (expected-args 1 args :count :at-least)
	      (arithmetic-1 #'- args))
           (* (expected-args 0 args :count :at-least)
	      (arithmetic-0 #'* args multiplication-identity))
           (/ (expected-args 1 args :count :at-least)
	      (arithmetic-1 #'/ args))
           (otherwise (simple-apply (simple-eval proc env) args env)))) ; Must find function definition in environment.
        ((eq (car proc) 'lambda)
         (destructuring-bind (lambda-params . body) (rest proc)
           (let ((env1 (augment-environment lambda-params args env)))
             (do ((body (rest body) (rest body))
                  (result (simple-eval (first body) env1) (simple-eval (first body) env1)))
                 ((endp body) result)))) )
           ;;;
           ;;;    This is inadequate because DOLIST must return value of final expression!
           ;;;    
           ;; (let ((env1 (augment-environment lambda-params args env))) ; Operators in body must resolve to primitives.
           ;;   (dolist (form body)
           ;;     (simple-eval form env1)))) )

           ;; (simple-eval body (augment-environment lambda-params args env)))) ; Operators in body must resolve to primitives.
         ;; (simple-eval (caddr proc)
         ;;              (augment-environment (cadr proc) args env)))
        ((eq (car proc) 'defun) ; Fix!
         (simple-apply (caddr proc) args (cons (cons (cadr proc) (caddr proc)) env)))) )

(defun expected-args (n args &key (count :exact))
  (case count
    (:exact (assert (= (length args) n) (n args) "Function called with wrong number of args: ~D (Expected ~D)" (length args) n))
    (:at-least (assert (>= (length args) n) (n args) "Function called with wrong number of args: ~D (Expected ≥ ~D)" (length args) n))))

;;;
;;;    Relational operator that accepts 1+ args (=, <, >, <=, >=)
;;;    For > 2 args, underlying operator is applied pairwise to args from L to R.
;;;    
(defun relational (op args)
  (destructuring-bind (x &optional (y nil y-supplied-p) &rest more) args
    (if y-supplied-p
	(and (funcall op x y) 
	     (or (null more)
		 (relational op (cons y more))))
        t)))

;;;
;;;    Arithmetic operator that accepts 0+ args (+, *) with appropriate identity for 0 args.
;;;    Single arg returned "as is".
;;;    For > 2 args, underlying operator is applied pairwise to args from L to R.
;;;    
(defun arithmetic-0 (op args identity)
  (arithmetic-1 op (if (null args) (list identity) args)))

;;;
;;;    Arithmetic operator that accepts 1+ args (-, /).
;;;    Single arg evaluates to appropriate inverse.
;;;    For > 2 args, underlying operator is applied pairwise to args from L to R.
;;;    
(defun arithmetic-1 (op args)
  (destructuring-bind (x &optional (y nil y-supplied-p) &rest more) args
    (if y-supplied-p
	(if (null more)
	    (funcall op x y)
	    (arithmetic-1 op (cons (funcall op x y) more)))
        (funcall op x))))

;;;
;;;    Add corresponding param/value pairs to front of environment.
;;;
;;;    - Will shadow existing variables with same names.
;;;    - Extra variables w/o corresponding values get NIL as a value.
;;;    
(defun augment-environment (params values env)
  (cond ((endp params) env)
        ((endp values) (acons (first params) nil (augment-environment (rest params) values env))) ; Final clause implicitly does this!!
        (t (acons (first params) (first values) (augment-environment (rest params) (rest values) env)))) )


