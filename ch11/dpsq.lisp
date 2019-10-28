;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp isn't a language, it's a building material.
;;;;   -- Alan Kay
;;;;
;;;;   Name:               dpsq.lisp
;;;;
;;;;   Started:            Mon Sep 30 18:19:08 2019
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
;;;;   Notes: Slade exercise 11.10.10
;;;;
;;;;   I fixed Slade's description so that updating SPOUSE property removes previous spouse's SPOUSE property.
;;;;   
;;;;   Deficiencies:
;;;;   - Adding child to node should add child to SPOUSE node?
;;;;   - "                                        SIBLING nodes?
;;;;
;;;;
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :dpsq (:use :common-lisp :test))

(in-package :dpsq)

;;;
;;;    Define PropertieS
;;;    
;; (defun dps (symbol &rest plist)
;;   (loop for (property value) on plist by #'cddr
;;         do (ddput symbol property value)))

;;;
;;;    New approach...treat +LAMBDA-PROPERTY as alternative to DDPUT
;;;    
(defun dps (node &rest plist)
  (loop for (property value) on plist by #'cddr
        do (let ((fn (isa-get property '+lambda-property))) ; Why ISA-GET????
             (if fn
               (funcall fn node property value)
               (ddput node property value)))) )

;;;
;;;    Define PropertieS-Quoted (See Slade style.)
;;;    
(defmacro dpsq (node &rest plist)
  `(dps ',node ,@(mapcar #'(lambda (elt) `',elt) plist)))

(defun isa-get (node property)
  (cond ((null node) nil)
        ((get node property))
        ((isa-get (get node 'isa) property))))

;;;
;;;    Data-driven put
;;;    
;; (defun ddput (symbol property value &optional recursive-p)
;;   (let ((meta-properties (symbol-plist property)))
;;     (unless (null meta-properties)
;;       (loop for (p v) on meta-properties by #'cddr
;;             do (process-meta-property p v symbol value)))
;;     (when (isa-get property '+save-property)
;;       (let ((old-value (get symbol property)))
;;         (when old-value
;;           (let ((save-node (gensym)))
;;             (ddput symbol 'save-node save-node)
;;             (ddput save-node property old-value t)
;;             (remprop old-value property)))) )
;;     (when (isa-get property '+invert-value)
;;       (unless recursive-p
;;         (ddput value property symbol t)))
;;     (when (isa-get property '+invert-property)
;;       (unless recursive-p
;;         (ddput property value symbol t)))
;;     (unless (getf meta-properties '+lambda-property)
;;       (if (isa-get property '+multiple-values)
;;           (pushnew value (get symbol property))
;;           (setf (get symbol property) value)))) )

;; (defun process-meta-property (meta-p meta-v symbol value)
;;   (case meta-p
;;     (+invert-onto (ddput value meta-v symbol))
;;     (+lambda-property (funcall meta-v symbol meta-p value))))

;;;
;;;    +LAMBDA-PROPERTY never reaches here.
;;;    
(defun ddput (node property value &optional recursive-p)
  (let ((meta-properties (symbol-plist property)))
    (unless (null meta-properties)
      (loop for (p v) on meta-properties by #'cddr
            do (process-meta-property p v node value)))
    (when (isa-get property '+save-property)
      (let ((old-value (get node property)))
        (when old-value
          (let ((save-node (gensym)))
            (ddput node 'save-node save-node)
            (ddput save-node property old-value t)
            (remprop old-value property)))) )
    (when (isa-get property '+invert-value)
      (unless recursive-p
        (ddput value property node t)))
    (when (isa-get property '+invert-property)
      (unless recursive-p
        (ddput property value node t)))
    (if (isa-get property '+multiple-values)
        (pushnew value (get node property)) ; Relies on missing property being empty list...
        (setf (get node property) value))))

(defun process-meta-property (meta-p meta-v node value)
  (case meta-p
    (+invert-onto (ddput value meta-v node))))

;;;
;;;    Slade's mess (See dpsq-slade.lisp)
;;;

;;;
;;;    Pretty-print properties
;;;    
(defun ppp (node)
  (let ((props (symbol-plist node)))
    (format t "~S~%" node)
    (loop for (property value) on props by #'cddr
          do (format t "~4T~S~20T~S~%" property value))))

;;;
;;;    Pretty-print properties quoted
;;;    
(defmacro pppq (node)
  `(ppp ',node))

(unless (boundp '*hierarchy*)
  (defvar *hierarchy* '((jane isa programmer sex female income sixy-k)
                        (john isa programmer sex male ingests junk-food)
                        (programmer isa person income fifty-k)
                        (person isa mammal)
                        (mammal isa organism)
                        (organism ingests (food air))))
  
  (dolist (node *hierarchy*)
    (apply #'dps node)))

(unless (boundp '*family*)
(trace ddput process-meta-property)
  (defvar *family* '((isa +invert-onto instances)
                     (instances +multiple-values t)
                     (*relationship +multiple-values t)
                     (relationship +save-property t)
                     (children isa *relationship)
                     (sibling isa *relationship +invert-value t)
                     (father isa relationship)
                     (spouse isa relationship +invert-value t)
                     (joe-jr father joe-sr spouse mary children pat children sue)))
  (dolist (node *family*)
    (apply #'dps node)))

(defun add-sibling ()
  (unless (boundp '*sibling*)
    (defvar *sibling* '((mary sibling dorothy sibling arthur)))
    (dolist (node *sibling*)
      (apply #'dps node))))

(defun remarry ()
  (unless (boundp '*remarry*)
    (defvar *remarry* '((joe-jr spouse louise children jackie)))
    (dolist (node *remarry*)
      (apply #'dps node))))

(defun add-jobs ()
  (unless (boundp '*jobs*)
    (defvar *jobs* '((job +invert-property t +multiple-values t)
                     (plumber isa job)
                     (carpenter isa job)
                     (joe-jr job plumber job carpenter)))
    (dolist (node *jobs*)
      (apply #'dps node))))

(defun add-son ()
  (unless (boundp '*son*)
    (defvar *son* `((son +lambda-property ,#'(lambda (node property value)
                                               (declare (ignore property))
                                               (ddput node 'children value)
                                               (dps value 'sex 'male 'father node)))
                    (joe-jr son lester)))
    (dolist (node *son*)
      (apply #'dps node))))






;; (defun ddput (symbol property value)
;;   (let ((meta-properties (symbol-plist property)))
;;     (if (null meta-properties)
;;         (setf (get symbol property) value)
;;         (let ((isa (getf meta-properties 'isa)))
;;           (if (null isa)
;;               (setf (get symbol property) value)
;;               (let ((isa-props (symbol-plist 'isa)))
;;                 (loop for (isa-property isa-value) on isa-props by #'cddr
;;                       do (process-isa-prop isa-property isa-value property isa)
;;                 (setf (get symbol property) value)))) ))))

;; (defun ddput (symbol property value)
;;   (let ((meta-properties (symbol-plist property)))
;;     (unless (null meta-properties)
;;       (loop for (p v) on meta-properties by #'cddr
;;             do (process-meta-property p v symbol value)))
;;     (cond ((isa-get property '+multiple-values)
;;            (pushnew value (get symbol property)))
;;           ((isa-get property '+save-property)
;;            (when (get symbol property)
;;              (let ((save-node (gensym)))
;;                (setf (get symbol 'save-node) save-node
;;                      (get save-node property) (get symbol property))))
;;            (setf (get symbol property) value))
;;           (t (setf (get symbol property) value)))) )

;; (defun process-isa-prop (isa-property isa-value property isa)
;;   (case isa-property
;;     (+invert-onto (pushnew property (get isa isa-value)))) )

