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
;;;;
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :simple-eval (:use :common-lisp :test))

(in-package :simple-eval)

;;;
;;;    ENV is dotted-pair alist of "bindings". See TEST-SIMPLE-EVAL below.
;;;    
(defun simple-eval (exp &optional env)
  (cond ((consp exp)
	 (cond ((simple-special-operator-p (first exp)) ; Consider macros as special operators...
		(special-eval exp env))
	       ;; ((lambda-expression-p exp)
	       ;; 	(simple-lambda exp env))
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
;		 (dolist (clause clauses)
;		   (simple-eval clause env))
;		 (simple-eval-list clauses env)
;		 (progn (simple-eval clause env) (simple-eval-
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
	   (print (expected-args 1 args)
		  (print (first args)))
	   (+ (destructuring-bind (&optional (x nil x-supplied-p) &rest xr) args ; Proper left fold
		(if x-supplied-p
		    (destructuring-bind (&optional (y nil y-supplied-p) &rest yr) xr
		      (if y-supplied-p
			  (if (null yr)
			      (+ x y)
			      (simple-apply proc (cons (+ x y) yr) env))
                          x))
		    0)))
	   (- (destructuring-bind (x &optional (y nil y-supplied-p) &rest yr) args
		(if y-supplied-p
		    (if (null yr)
			(- x y)
		        (simple-apply proc (cons (- x y) yr) env))
		        ;; (- (- x y) (simple-apply proc yr env)))
		    (- x))))
	   (* (destructuring-bind (&optional (x nil x-supplied-p) &rest xr) args
		(if x-supplied-p
		    (destructuring-bind (&optional (y nil y-supplied-p) &rest yr) xr
		      (if y-supplied-p
			  (if (null yr)
			      (* x y)
			      (simple-apply proc (cons (* x y) yr) env))
                          x))
		    1)))
	   (/ (destructuring-bind (x &optional (y nil y-supplied-p) &rest yr) args
		(if y-supplied-p
		    (if (null yr)
			(/ x y)
		        (simple-apply proc (cons (/ x y) yr) env))
		    (/ x))))
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

(defun expected-args (n args)
  (assert (= n (length args)) (n args) "Function called with wrong number of args: ~D (Expected ~D)" (length args) n))

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

(deftest test-augment-environment ()
  (check
   (equal (augment-environment '(x y) '(2 5) '((x . 0))) '((x . 2) (y . 5) (x . 0)))
   (equal (augment-environment '(x y) '(2) '((x . 0))) '((x . 2) (y . nil) (x . 0)))) )

(deftest test-simple-eval ()
  (check
   (equal (simple-eval 9) 9)
   (equal (simple-eval "pung") "pung")
   (equal (simple-eval #\a) #\a)
   (equal (simple-eval t) t)
   (equal (simple-eval nil) nil)
   (equal (simple-eval pi) pi)
   (equal (simple-eval :t) :t)
   (equal (simple-eval '(quote pung)) 'pung)
   (equal (simple-eval '(car '(a b c))) 'a)
   (equal (simple-eval '(cdr '(a b c))) '(b c))
   (equal (simple-eval '(cons (car '(a b c)) (cdr '(a b c)))) '(a b c))
   (not (simple-eval '(zerop x) '((x . 5) (zerop . (lambda (x) (eq x 0)))) ))
   (simple-eval '(zerop x) '((x . 0) (zerop . (lambda (x) (eq x 0)))) )
   (simple-eval '(atom 99))
;   (simple-eval '(atom (vector 1 2 3)))
   (simple-eval '(eq 'pung 'pung))
   (equal (simple-eval '(cond ((zerop x) (print 'pung) (print (eq 5 3)) (car '(a b c))) (t 'foo)) '((x . 0) (zerop . (lambda (x) (eq x 0)))) ) 'a)
   (equal (simple-eval '(cond ((zerop x) (print 'pung) (print (eq 5 3)) (car '(a b c))) (t 'foo)) '((x . 1) (zerop . (lambda (x) (eq x 0)))) ) 'foo)
   (equal (simple-eval '(cond ((zerop x) (print 'pung) (print (eq 5 3)) (car '(a b c)))) '((x . 1) (zerop . (lambda (x) (eq x 0)))) ) nil)
   (equal (simple-eval '(if (zerop x) (cons x '()) z) '((x . 0) (z . 9) (zerop . (lambda (x) (eq x 0)))) ) '(0))
   (equal (simple-eval '(if (zerop x) (cons x '()) z) '((x . 1) (z . 9) (zerop . (lambda (x) (eq x 0)))) ) 9)
   (simple-eval '(f (car '(0 1 2))) '((f . (lambda (x) (zerop x))) (zerop . (lambda (y) (eq y 0)))) )
   (not (simple-eval '(f (car '(3 0 1 2))) '((f . (lambda (x) (zerop x))) (zerop . (lambda (y) (eq y 0)))) ))))
