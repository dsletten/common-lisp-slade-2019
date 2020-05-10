;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               cl-eval.lisp
;;;;
;;;;   Started:            Tue Apr 14 20:10:59 2020
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;     My fixed version of Slade's CL-EVAL. (Incorporates some ideas from my SIMPLE-EVAL.)
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
;;;;    TODO
;;;;   - Lexical functions #'foo
;;;;     Two cases with LAMBDA:
;;;;     1. LAMBDA expression: ((lambda (x) (+ x 3)) 4) Create binding for X. Evaluate as PROGN
;;;;     2. Function object in environment: #'(lambda (x) (+ x 3))
;;;;   - Really rely on built-in functions for everything???
;;;;
;;;;
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :cl-eval (:use :common-lisp :test))

(in-package :cl-eval)

(defun cl-eval (form &optional env)
  (cond ((consp form) ; Four flavors of CONS as form: special form/macro form/function form (CAR is symbol) or lambda form (CAR is list)
         (destructuring-bind (operator . params) form
           (cond ((symbolp operator)
                  (if (cl-special-operator-p operator) ; Consider macros as special operators...
                      (special-eval operator params env)
                      (cl-eval-call operator params env))) ; Arbitrary function
                 ((and (consp operator) (eq (first operator) 'lambda))
                  (cl-eval-lambda operator params env)) ; Lambda form     Skip CL-EVAL-CALL????  PARAMS must be evaluated first?!?!
;                  (cl-eval-call operator params env)) ; Lambda form
                 (t (error "~S is not a function name; try using a symbol instead" operator)))) )
        ((symbolp form) (value form env)) ; Have to treat PI as symbol rather than constant form.
        ((constantp form) form) ; Self-evaluating
        (t (error "How did we get here?!? This is not Lisp: ~S" form))))

(defun cl-special-operator-p (obj)
  (case obj
    ((quote function cond if and or progn let let* defun setq case) t)
    (otherwise nil)))

;;;
;;;    Destructuring of PARAMS enforces required number of args for most...
;;;    
(defun special-eval (operator params env)
  (ecase operator
    (quote (expected-args 1 params :msg "wrong number of args to QUOTE:") (first params))
    (function (cl-eval-function params env))
;    (function (expected-args 1 params :msg "wrong number of args to FUNCTION:") (symbol-function (first params))) ; Not right for lexical functions!!
    (cond (cl-eval-cond params env))
    (if (cl-eval-if params env))
    (and (cl-eval-and params env))
    (or (cl-eval-or params env))
    (progn (cl-eval-progn params env))
    (let (cl-eval-let params env))
    (let* (cl-eval-let* params env))
    (defun (cl-eval-defun params env))
    (setq (cl-eval-setq params env))
    (case (cl-eval-case params env))))

(defun cl-eval-call (operator params env)
  (let ((func (cl-eval operator env)))
    (if (and (consp func) (eq 'lambda (car func)))
        (cl-eval-lambda func params env)
        (apply func (mapcar #'(lambda (arg) (cl-eval arg env)) params)))) )
;; (defun cl-eval-call (operator params env)
;;   (let* ((func (cl-eval operator env))
;;          (func2 (if (and (consp func) (eq 'lambda (car func)))
;;                     (cl-eval-lambda func env)
;;                     func)))
;; ;    (print func2)
;;     (apply func2 (mapcar #'(lambda (arg) (cl-eval arg env)) params))))


;;;
;;;    Not quite right...
;;;    
(defun cl-eval-function (params env)
;(format t "Yo: ~A~%" params)
  (destructuring-bind (f) params
    (cond ((symbolp f) (let ((func (cl-eval f env)))
                         ))
          ((and (consp f) (eq 'lambda (first f))) f) ; Throw back and let LAMBDA macro do its thing???
          (t (error "What kind of function is this: ~A~%" params)))) )

;; (defun cl-eval-if (exp env)
;;   (if (cl-eval (cadr exp) env)
;;       (cl-eval (caddr exp) env)
;;       (cl-eval (cadddr exp) env)))

(defun cl-eval-if (body env)
  (destructuring-bind (test then &optional else) body
    (if (cl-eval test env)
        (cl-eval then env)
        (cl-eval else env))))

(defun cl-eval-cond (body env)
  (cond ((null body) nil)
        (t (destructuring-bind ((test . clauses) . more) body
             (let ((test-result (cl-eval test env)))
               (if test-result
                   (if (null clauses)
                       test-result
                       (cl-eval-progn clauses env))
                   (cl-eval-cond more env)))) )))

;; (defun simple-progn (forms env)
;;   (labels ((cl-progn (forms env result) ; Need ENV as param? Can a form change env?
;;              (if (endp forms)
;;                  (values-list result)
;;                  (cl-progn (rest forms) env (multiple-value-list (simple-eval (first forms) env)))) ))
;;     (if (null forms)
;;         nil
;;         (cl-progn (rest forms) env (multiple-value-list (simple-eval (first forms) env)))) ))


;;;
;;;    This is my analog to Slade's CL-EVAL-SEQUENCE
;;;    
(defun cl-eval-progn (forms env)
  (if (null forms)
      nil
      (do ((body (rest forms) (rest body))
           (result (multiple-value-list (cl-eval (first forms) env)) 
                   (multiple-value-list (cl-eval (first body) env))))
          ((endp body) (values-list result)))) )

;; (defun cl-eval-progn (exp env)
;;   (cl-eval-sequence exp env)) ; ???
;; ;  (cl-eval-sequence (cdr exp) env)) ; ???


;;;    Only LAMBDA left uses this...
;; (defun cl-eval-sequence (exps env) ; ???
;;   (cond ((null (cdr exps)) (cl-eval (car exps) env))
;;         (t (cl-eval (car exps) env)
;;            (cl-eval-sequence (cdr exps) env))))

;;;
;;;    Not quite right...
;;;   
;; (defun cl-eval-sequence (exps env)
;;   (do ((exps exps (rest exps))
;;        (result nil (cl-eval (first exps))))
;;       ((endp exps) result)))

(defun cl-eval-or (forms env)
  (if (null forms)
      nil
      (do ((body (rest forms) (rest body))
           (result (multiple-value-list (cl-eval (first forms) env)) 
                   (multiple-value-list (cl-eval (first body) env))))
          ((endp body) (values-list result))
        (when (first result)
          (return (first result)))) ))

(defun cl-eval-and (forms env) ; Expand to IF?!?!
  (if (null forms)
      t
      (do ((body (rest forms) (rest body))
           (result (multiple-value-list (cl-eval (first forms) env)) 
                   (multiple-value-list (cl-eval (first body) env))))
          ((endp body) (values-list result))
        (unless (first result)
          (return nil)))) )

;;;
;;;    LET/LET* are implicit PROGNs...
;;;
;;;    Evaluate all INIT-FORMs first.
;;;
;;;    This handles the full LET syntax:
;;;    (let (x
;;;          (y)
;;;          (z 10))
;;;      ...)
;;;    
(defun cl-eval-let (form env)
  (destructuring-bind (bindings . body) form
    (let ((new-env (augment-environment-bindings bindings env)))
      (cl-eval-progn body new-env))))

(defun augment-environment-bindings (bindings env)
  (labels ((augment (bindings vars vals)
             (if (endp bindings)
                 (bind-variables (nreverse vars) (nreverse vals) env)
                 (destructuring-bind (binding . more) bindings
                   (if (symbolp binding)
                       (augment more (cons binding vars) (cons nil vals))
                       (destructuring-bind (var &optional (value nil value-p)) binding
                         (if value-p
                             (augment more (cons var vars) (cons (cl-eval value env) vals))
                             (augment more (cons var vars) (cons nil vals)))) )))) )
    (augment bindings '() '())))

;;;
;;;    Sequential bindings.
;;;    
(defun cl-eval-let* (forms env)
  (destructuring-bind (bindings . body) forms
    (let ((new-env (augment-environment-bindings* bindings env)))
      (cl-eval-progn body new-env))))

(defun augment-environment-bindings* (bindings env)
  (labels ((augment (bindings env)
             (if (endp bindings)
                 env
                 (destructuring-bind (binding . more) bindings
                   (if (symbolp binding)
                       (augment more (bind-variable binding nil env))
                       (destructuring-bind (var &optional (value nil value-p)) binding
                         (if value-p
                             (augment more (bind-variable var (cl-eval value env) env))
                             (augment more (bind-variable var nil env)))) )))) )
    (augment bindings env)))

(defun expected-args (n args &key (count :exact) (msg "Operator called with wrong number of args:"))
  (case count
    (:exact (assert (= (length args) n) (n args) "~A ~D (Expected ~D)" msg (length args) n))
    (:at-least (assert (>= (length args) n) (n args) "~A ~D (Expected ≥ ~D)" (length args) n))))


;;;
;;;    Renamed Slade's ENVIRONMENT class.
;;;    
(defclass binding ()
  ((id :initarg :id :reader id)
   (contents :initarg :contents :accessor contents)
   (next :initarg :next :accessor next))
  (:documentation "Class representing a binding and a link to the next binding."))

;; (defun make-environment (id val env localp)
;;   (let ((new (make-instance 'environment :id id :contents val :next nil)))
;;     (cond ((and env (not localp))
;;            (setf (next new) (next env))
;;            (setf (next env) new))
;;           ((and env localp)
;;            (setf (next new) env)))
;;     new))

;; (defun make-environment (id val env localp)
;;   (let ((new (make-instance 'environment :id id :contents val :next nil)))
;;     (when env
;;       (if localp
;;           (setf (next new) env)
;;           (setf (next new) (next env)
;;                 (setf (next env) new))))
;;     new))
;; (defun make-environment (id val env localp) ; Possible for LOCALP to be true with NULL ENV??!?!?
;;   (let ((new (make-instance 'environment :id id :contents val :next env)))
;;     (unless localp
;;       (setf (next new) (next env)
;;             (setf (next env) new)))
;;     new))

(defun make-binding (id val env)
  (let ((new (make-instance 'binding :id id :contents val :next nil)))
    (when env
      (setf (next new) (next env)
            (next env) new))
    new))
      
(defun make-local-binding (id val env)
  (make-instance 'binding :id id :contents val :next env))

(defmethod print-object ((env binding) str)
;(defmethod print-object ((env binding) (str stream))
  (format str "~%~A:~10T~A" (id env) (contents env))
  (when (next env)
    (print-object (next env) str)))

;;;
;;;    Try to find a binding containing ID.
;;;    If current binding does not hold ID, check the NEXT link. Otherwise NIL.
;;;
;;;    In case of SETQ/DEFUN (via SET-VALUE) create a new (global) binding if one does not exist.
;;;    
(defgeneric cl-env-lookup (env id createp))
(defmethod cl-env-lookup ((env binding) id createp)
  (cond ((eq (id env) id) env)
        ((next env) (cl-env-lookup (next env) id createp))
        (createp (make-binding id nil env))
        (t nil)))

(defmethod cl-env-lookup (env id createp)
  (declare (ignore env createp)) ; Literally ignores environment!!
  id)

;;;
;;;    This method specializes on general object. BINDING class defines as an accessor above.
;;;
;(defgeneric contents...
(defmethod contents (obj)
  (cond ((fboundp obj) (symbol-function obj))
;        ((functionp obj) obj)
        ((boundp obj) (symbol-value obj))
        (t obj)))

;;;
;;;   Duplicates?!?
;;;   
(defun bind-variables (ids values env)
  (cond ((consp ids)
         (bind-variables (rest ids) (rest values) (bind-variable (first ids) (first values) env))) ; Tail-recursive! Later vars shadow previous.
         ;; (bind-variable (car ids) (car values) (bind-variables (cdr ids) (cdr values) env))) ; Not tail-recursive. Wrong order?!?
        ((null ids) env)
        (t (bind-variable ids values env)))) ; How do we get here?????

;; (defun bind-variables (ids values env)
;;   (cond ((null ids) env)
;;         ((consp ids)
;;          (bind-variable (car ids) (car values) (bind-variables (cdr ids) (cdr values) env)))
;;         (t (bind-variable ids values env))))

(defun bind-variable (identifier value env)
  (make-local-binding identifier value env))

(defun value (id env)
  (let ((result (cl-env-lookup env id nil)))
    (cond (result (contents result)) ; Value found in ENV or ENV is NIL => ID
          (t (contents id)))) ) ; Value of sth. else. Only get here if ENV is non-NIL but ID not found??

;;;
;;;    Traverses chain of bindings. Creates new (global) binding if existing one cannot be found.
;;;    (Changed Slade's parameter order!)
;;;    
(defun set-value (id val env)
  (setf (contents (cl-env-lookup env id t)) val))

; Does not handle full Lambda list: &optional, &key, &rest...
;(defun cl-eval-lambda (lambda-expression env)
(defun cl-eval-lambda (lambda-expression args env)
  (destructuring-bind (lambda-list . body) (rest lambda-expression)
    (cl-eval-progn body (bind-variables lambda-list args env))))

(defun cl-eval-setq (pair env)
  (destructuring-bind (var form) pair
    (set-value var (cl-eval form env) env)))

;;;
;;;    Unlike SIMPLE-EVAL this follows Lisp DEFUN syntax! (Not Scheme)
;;;    Returns NAME as value!
;;;    
(defun cl-eval-defun (exp env)
  (destructuring-bind (name lambda-list . body) exp
    (set-value name `(lambda ,lambda-list ,@body) env)
    name))

;;;
;;;    CLHS:
;;;    case keyform {normal-clause}* [otherwise-clause] => result*
;;;    OTHERWISE-CLAUSE must be last!
;;;
;;;    (case test-key
;;;    {((key*) form*)}*)
;;;    ==
;;;    (let ((#1=#:g0001 test-key))
;;;      (cond {((member #1# '(key*)) form*)}*))
;;;      
(defun cl-eval-case (forms env)
  (labels ((process-clauses (test-key clauses)
             (if (endp clauses) 
                 nil
                 (destructuring-bind ((keys . forms) . more) clauses
                   (cond ((and (null more) (otherwise-clause-p keys))
                          (cl-eval-progn forms env))
                         ((and (listp keys) (member test-key keys))
                          (cl-eval-progn forms env))
                         ((eql test-key keys)
                          (cl-eval-progn forms env))
                         (t (process-clauses test-key more)))) ))
           (otherwise-clause-p (key)
             (member key '(otherwise t))))
    (destructuring-bind (keyform . clauses) forms
      (let ((test-key (cl-eval keyform env)))
        (process-clauses test-key clauses)))) )

  ;; (destructuring-bind (keyform . clauses) forms
  ;;   (let ((test-key (cl-eval keyform env)))
  ;;     (dolist (clause clauses)
  ;;       (destructuring-bind (keys . forms) clause
  ;;         (cond ((listp keys)
  ;;                (when (member test-key keys :test #'eq)
  ;;                  (return (cl-eval-progn forms env))))
  ;;               (t (when (eq test-key keys)
  ;;                    (return (cl-eval-progn forms env)))) ))
    
