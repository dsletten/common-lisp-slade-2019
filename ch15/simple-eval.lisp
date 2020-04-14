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
;;;;   Thus--
;;;;   (simple-eval (read))
;;;;   (let ((f '(lambda (x) (* x 9)))) (f 2))
;;;;   
;;;;   A Lisp-2 would require 2 environments--function and variable bindings. (Or one environment split into 2 parts...)
;;;;
;;;;   https://en.wikipedia.org/wiki/Mathematical_operators_and_symbols_in_Unicode
;;;;
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :simple-eval (:use :common-lisp :test))

(in-package :simple-eval)

(defconstant addition-identity 0)
(defconstant multiplication-identity 1)

;;;    TODO: Closures, lexical scope, special variables
;;;          Don't continually expand environment on recursive calls to SIMPLE-EVAL

;;; !!!!!!!
;(dolist (outer '(car cdr)) (dolist (middle '(car cdr)) (dolist (inner '(car cdr)) (format t "(cxxxr . (lambda (l) (~A (~A (~A l)))) )~%" outer middle inner))))
;(dolist (outerest '(car cdr)) (dolist (outer '(car cdr)) (dolist (middle '(car cdr)) (dolist (inner '(car cdr)) (format t "(cxxxxr . (lambda (l) (~A (~A (~A (~A l)))) ))~%" outerest outer middle inner)))))
;(loop for i from 1 to 10 do (format t "(~:R . car)~%" i))

;(defvar *base-environment* `((t . t)
(defparameter *base-environment* `(;(t . t) ; Hack in SIMPLE-ENV-LOOKUP below
                                   ;(nil . nil)
                                   ;(pi . ,pi)
                                   (caar . (lambda (l) (car (car l))))
                                   (cadr . (lambda (l) (car (cdr l))))
                                   (cdar . (lambda (l) (cdr (car l))))
                                   (cddr . (lambda (l) (cdr (cdr l))))
                                   (caaar . (lambda (l) (car (car (car l)))) )
                                   (caadr . (lambda (l) (car (car (cdr l)))) )
                                   (cadar . (lambda (l) (car (cdr (car l)))) )
                                   (caddr . (lambda (l) (car (cdr (cdr l)))) )
                                   (cdaar . (lambda (l) (cdr (car (car l)))) )
                                   (cdadr . (lambda (l) (cdr (car (cdr l)))) )
                                   (cddar . (lambda (l) (cdr (cdr (car l)))) )
                                   (cdddr . (lambda (l) (cdr (cdr (cdr l)))) )
                                   (caaaar . (lambda (l) (car (car (car (car l)))) ))
                                   (caaadr . (lambda (l) (car (car (car (cdr l)))) ))
                                   (caadar . (lambda (l) (car (car (cdr (car l)))) ))
                                   (caaddr . (lambda (l) (car (car (cdr (cdr l)))) ))
                                   (cadaar . (lambda (l) (car (cdr (car (car l)))) ))
                                   (cadadr . (lambda (l) (car (cdr (car (cdr l)))) ))
                                   (caddar . (lambda (l) (car (cdr (cdr (car l)))) ))
                                   (cadddr . (lambda (l) (car (cdr (cdr (cdr l)))) ))
                                   (cdaaar . (lambda (l) (cdr (car (car (car l)))) ))
                                   (cdaadr . (lambda (l) (cdr (car (car (cdr l)))) ))
                                   (cdadar . (lambda (l) (cdr (car (cdr (car l)))) ))
                                   (cdaddr . (lambda (l) (cdr (car (cdr (cdr l)))) ))
                                   (cddaar . (lambda (l) (cdr (cdr (car (car l)))) ))
                                   (cddadr . (lambda (l) (cdr (cdr (car (cdr l)))) ))
                                   (cdddar . (lambda (l) (cdr (cdr (cdr (car l)))) ))
                                   (cddddr . (lambda (l) (cdr (cdr (cdr (cdr l)))) ))
                                   (first . car) ; Should this work?!?!
                                   (second . cadr)
                                   (third . caddr)
                                   (fourth . cadddr)
                                   (fifth . (lambda (l) (car (cddddr l))))
                                   (sixth . (lambda (l) (cadr (cddddr l))))
                                   (seventh . (lambda (l) (caddr (cddddr l))))
                                   (eighth . (lambda (l) (cadddr (cddddr l))))
                                   (ninth . (lambda (l) (car (cddddr (cddddr l)))) )
                                   (tenth . (lambda (l) (cadr (cddddr (cddddr l)))))
                                   (rest . cdr)
                                   (append . (lambda (l1 l2)
                                               (if (null l1)
                                                   l2
                                                   (cons (first l1) (append (rest l1) l2)))) )
                                   (subst . (lambda (new old tree)
                                              (cond ((eql tree old) new)
                                                    ((atom tree) tree)
                                                    (t (cons (subst new old (car tree))
                                                             (subst new old (cdr tree)))) )))
                                   (member . (lambda (obj l)
                                               (cond ((null l) '())
                                                     ((eql obj (first l)) l)
                                                     (t (member obj (rest l)))) ))
                                   (assoc . (lambda (item alist)
                                              (cond ((null alist) '())
                                                    ((eql item (caar alist)) (car alist))
                                                    (t (assoc item (rest alist)))) ))
                                   (reverse . (lambda (l) ; Can't do this without TR aux function?
                                                (reverse-aux l '())))
                                   (reverse-aux . (lambda (l result)
                                                    (if (null l)
                                                        result
                                                        (reverse-aux (rest l) (cons (first l) result)))) )
                                   (mapcar . (lambda (f l)
                                               (if (null l)
                                                   '()
                                                   (cons (funcall f (first l)) (mapcar f (rest l)))) ))
                                   (reduce . (lambda (f l)
                                               (if (null l)
                                                   (funcall f)
                                                   (foldl f (first l) (rest l)))) )
                                   (foldl . (lambda (f acc l)
                                              (if (null l)
                                                  acc
                                                  (foldl f (funcall f acc (first l)) (rest l)))) )))



;;;
;;;    ENV is dotted-pair alist of "bindings". See test-simple-eval.lisp for examples.
;;;    Don't augment env on recursive calls?!?!
;;;    
;;;    CLHS 3.1.2.1 Form Evaluation
;;;    Forms fall into three categories: symbols, conses, and self-evaluating objects.
;;;    
;(defun simple-eval (form &optional env)
;  (let ((new-env (augment-environment env *base-environment*)))
(defun simple-eval (form &optional (env *base-environment*))
  (let ((new-env env))
    (cond ((consp form) ; Four flavors of CONS as form: special form/macro form/function form (CAR is symbol) or lambda form (CAR is list)
           (destructuring-bind (operator . params) form
             (cond ((symbolp operator)
                    (if (simple-special-operator-p operator) ; Consider macros as special operators...
                        (special-eval operator params new-env)
                        (simple-apply operator (simple-eval-list params new-env) new-env))) ; Arbitrary function
                   ((and (consp operator) (eq (first operator) 'lambda))
                    (simple-apply operator (simple-eval-list params new-env) new-env)) ; Lambda form
                   (t (error "~S is not a function name; try using a symbol instead" operator)))) )
          ((symbolp form) (simple-env-lookup form new-env)) ; Have to treat PI as symbol rather than constant form.
          ((constantp form) form) ; Self-evaluating
          (t (error "How did we get here?!? This is not Lisp: ~S" form)))) )

(defun simple-special-operator-p (obj)
  (case obj
    ((quote function cond if and or progn let let* defun) t)
    (otherwise nil)))

(defun special-eval (operator params env)
  (ecase operator
    (quote (expected-args 1 params :msg "wrong number of args to QUOTE:") (first params))
    (function (expected-args 1 params :msg "wrong number of args to FUNCTION:") (first params))
    (cond (simple-cond params env))
    (if (simple-if params env))
    (and (simple-and params env))
    (or (simple-or params env))
    (progn (simple-progn params env))
    (let* (simple-let* params env))
    (let (simple-let params env))
    (defun (simple-defun params env))))

;; (defun simple-env-lookup (id env)
;;   (cdr (assoc id env)))

(defun simple-env-lookup (id env)
  (if (boundp id)       ; Hack for fundamental values, e.g., 
      (symbol-value id) ;   T, NIL, PI, keyword symbols, etc... regardless of ENV...
      (let ((entry (assoc id env)))
        (if (null entry)
            (error "No binding for ~A in environment." id)
            (cdr entry)))) )

;;;
;;;    Evaluate list of test clauses.
;;;    Must include final T clause!! <-- Nope
;;;    
;; (defun simple-cond (body env)
;;   (cond ((null body) nil)
;;         (t (destructuring-bind ((test . clauses) . more) body
;;              (if (simple-eval test env) 
;;                  (do ((clauses (rest clauses) (rest clauses))
;;                       (result (simple-eval (first clauses)) (simple-eval (first clauses))))  ; What about ENV??
;;                      ((endp clauses) result))
;; ;                (dolist (clause clauses)
;; ;                  (simple-eval clause env))
;; ;                (simple-eval-list clauses env)
;; ;                (progn (simple-eval clause env) (simple-eval-
;;                  (simple-cond more env)))) ))
(defun simple-cond (body env)
  (cond ((null body) nil)
        (t (destructuring-bind ((test . clauses) . more) body
             (let ((test-result (simple-eval test env)))
               (if test-result
                   (if (null clauses)
                       test-result
                       (simple-progn clauses env))
                   (simple-cond more env)))) )))

(defun simple-if (body env)
  (destructuring-bind (test then &optional else) body
    (if (simple-eval test env)
        (simple-eval then env)
        (simple-eval else env))))

;; fun {All Xs F}
;;    case Xs of nil then true
;;    [] X|Xr then {F X} andthen {All Xr F}
;;    end
;; end
;; (defun simple-and (body env)
;;   (if (endp body)
;;       t
;;       (if (simple-eval (first body) env)
;; 	  (simple-and (rest body) env)
;; 	  nil)))

;; (and (gethash :foo (make-hash-table))) => NIL; NIL
;; (and (gethash :foo (make-hash-table)) 8) => NIL
(defun simple-and (body env) ; Expand to IF?!?!
  (labels ((cl-and (body env result)
             (if (endp body)
                 (values-list result)
                 (if (null (first result))
                     nil
                     (cl-and (rest body) env (multiple-value-list (simple-eval (first body) env)))) )))
    (if (endp body)
        t
        (cl-and (rest body) env (multiple-value-list (simple-eval (first body) env)))) ))
                 

;; fun {Some Xs F}
;;    case Xs of nil then false
;;    [] X|Xr then {F X} orelse {Some Xr F}
;;    end
;; end
;; (defun simple-or (body env)
;;   (if (endp body)
;;       nil
;;       (if (simple-eval (first body) env)
;; 	  t
;; 	  (simple-or (rest body) env))))

;; (simple-eval '(or (truncate 3 4))) => 0; 3
;; (simple-eval '(or (truncate 3 4) (> 2 3))) => 0

(defun simple-or (body env)
  (labels ((cl-or (body env result)
             (if (endp body)
                 (values-list result)
                 (if (null (first result))
                     (cl-or (rest body) env (multiple-value-list (simple-eval (first body) env)))
                     (first result)))) ) ; Only primary value (not necessarily T) unless last form.
    (if (endp body)
        nil
        (cl-or (rest body) env (multiple-value-list (simple-eval (first body) env)))) ))

;;;    Figure out DO loops for COND/LAMBDA!



;;;
;;;    OR/AND are like conditional PROGN--may or may not evaluate all subforms...
;;;    
(defun simple-progn (forms env)
  (labels ((cl-progn (forms env result) ; Need ENV as param? Can a form change env?
             (if (endp forms)
                 (values-list result)
                 (cl-progn (rest forms) env (multiple-value-list (simple-eval (first forms) env)))) ))
    (if (null forms)
        nil
        (cl-progn (rest forms) env (multiple-value-list (simple-eval (first forms) env)))) ))

(defun simple-progn (forms env)
  (if (null forms)
      nil
      (do ((body (rest forms) (rest body))
           (result (simple-eval (first forms) env) (simple-eval (first body) env)))
          ((endp body) result))))

; Support vars w/o explicit initializers: (let (x (y)) ...)

;;;
;;;    LET/LET* are implicit PROGNs...
;;;    
(defun simple-let (form env)
  (destructuring-bind (bindings . body) form
    (simple-progn body (augment-environment-bindings bindings env))))

(defun simple-let* (form env)
  (destructuring-bind (bindings . body) form
    (simple-progn body (augment-environment-bindings* bindings env))))

(defun simple-defun (form env)
  (declare (ignore env)) ; How to capture ENV????
  (destructuring-bind (name lambda-list . body) form
    (push `(,name . (lambda ,lambda-list ,@body)) *base-environment*)))

;;;
;;;    This is a PROGN that keeps all its results???
;;;    
(defun simple-eval-list (exp env)
  (cond ((null exp) nil)
        (t (cons (simple-eval (car exp) env)
                 (simple-eval-list (cdr exp) env)))) )

;;;
;;;    Only LAMBDA really needs ENV here?! It may add new bindings to the environment in which its body is evaluated.
;;;    Also function without predefined CASE. Must look up function def in ENV.
;;;    
(defun simple-apply (operator args env)
  (cond ((null operator) (error "Whaaa?"))
        ((atom operator)
         (case operator
           (car (expected-args 1 args)
                (car (first args)))
           (cdr (expected-args 1 args)
                (cdr (first args)))
           (cons (expected-args 2 args)
                 (cons (first args) (second args)))
           (list args)
           (atom (expected-args 1 args)
                 (atom (first args)))
           (null (expected-args 1 args)
                 (null (first args)))
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
           (funcall (expected-args 1 args :count :at-least)
                    (simple-apply (first args) (rest args) env))
           (print (expected-args 1 args)
                  (print (first args)))
           (truncate (expected-args 1 args :count :at-least)
                     (apply #'truncate args))
           (= (expected-args 1 args :count :at-least)
	      (relational #'= args))
           ;; (/= (expected-args 1 args :count :at-least)
           ;;     (destructuring-bind (x &optional (y nil y-supplied-p) &rest more) args
           ;;    (if y-supplied-p
           ;;        (and (/= x y) 
           ;;             (or (null more)
           ;;                 (simple-apply operator (cons y more) env)))
           ;;        t)))
           (/= (expected-args 1 args :count :at-least) ; This one is tricky: With n args -> (n 2) pairwise comparisons!!
               (destructuring-bind (x &optional (y nil y-supplied-p) &rest more) args
                 (if y-supplied-p
                     (if (null more)
                         (/= x y) 
                         (every #'identity (simple-eval-list (mapcon #'(lambda (l) (mapcar #'(lambda (elt) (list operator (first l) elt)) (rest l))) args) env))) ; Don't really need ENV. All args eval'd already.
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
           (otherwise (let ((f (simple-eval operator env))) ; Must find function definition in environment.
                        (if (null f)
                            (error "undefined function ~S" operator)
                            (simple-apply f args env)))) ))
        ((eq (first operator) 'lambda)
         (destructuring-bind (lambda-list . body) (rest operator)
           (let ((env1 (augment-environment (pairlis lambda-list args) env))) ; Does not handle full Lambda list: &optional, &key, &rest...
             (simple-progn body env1)))) ))

(defun expected-args (n args &key (count :exact) (msg "Operator called with wrong number of args:"))
  (case count
    (:exact (assert (= (length args) n) (n args) "~A ~D (Expected ~D)" msg (length args) n))
    (:at-least (assert (>= (length args) n) (n args) "~A ~D (Expected ≥ ~D)" (length args) n))))

;;;
;;;    Relational operator that accepts 1+ args (=, <, >, <=, >=)
;;;    For > 2 args, underlying operator is applied pairwise to args from L to R.
;;;    /= is a special case!
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
;;;    Two ways to add bindings to environment:
;;;    1. LAMBDA. Parameter list and argument list are separate. Both lists must be same length for required params.
;;;    2. LET. Bindings listed as pairs of variables/values. Possible uninitialized vars: X (Y)
;;;    

(defun augment-environment-bindings (bindings env)
  (labels ((add-pair (var val pairs)
             (cons (cons var val) pairs))
           (augment-aux (bindings pairs)
             (if (endp bindings)
                 (augment-environment pairs env)
                 (destructuring-bind (binding . more) bindings
                   (if (symbolp binding)
                       (augment-aux more (add-pair binding nil pairs))
                       (destructuring-bind (var &optional (value nil value-p)) binding
                         (if value-p
                             (augment-aux more (add-pair var (simple-eval value env) pairs))
                             (augment-aux more (add-pair var value pairs)))) )))) )
    (augment-aux bindings '())))

;;;
;;;    Sequential bindings (LET*).
;;;    Does too much work repeatedly calling AUGMENT-ENVIRONMENT?
;;;    
(defun augment-environment-bindings* (bindings env)
  (labels ((augment-aux (bindings env)
             (if (endp bindings)
                 env
                 (destructuring-bind (binding . more) bindings
                   (if (symbolp binding)
                       (augment-aux more (augment-environment (list (cons binding nil)) env))
                       (destructuring-bind (var &optional (value nil value-p)) binding
                         (if value-p
                             (augment-aux more (augment-environment (list (cons var (simple-eval value env))) env))
                             (augment-aux more (augment-environment (list (cons var nil)) env)))) )))) )
    (augment-aux bindings env)))

;;;
;;;    Add corresponding param/value pairs to front of environment.
;;;
;;;    - Will shadow existing variables with same names.
;;;    - Extra variables w/o corresponding values get NIL as a value.
;;;    
;; (defun augment-environment (params values env)
;;   (cond ((endp params) env)
;;         ((endp values) (acons (first params) nil (augment-environment (rest params) values env))) ; Final clause implicitly does this!!
;;         (t (acons (first params) (first values) (augment-environment (rest params) (rest values) env)))) )

(defun augment-environment (new old)
;  (append new old))
  (append (remove-if #'(lambda (pair) (constantp (first pair))) new) old)) ; Prevent shadowing of constants, e.g., T, NIL
