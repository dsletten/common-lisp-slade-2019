;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp isn't a language, it's a building material.
;;;;   -- Alan Kay
;;;;
;;;;   Name:               dpsq2.lisp
;;;;
;;;;   Started:            Tue Oct 22 00:35:25 2019
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
;;;;   Notes: Redesign after understanding the spec!!
;;;;   I ignore Slade's use of +DDPROPS as a catlog of properties of interest.
;;;;
;;;;   subclass -> ISA -> superclass
;;;;   superclass -> INSTANCES -> subclass
;;;;
(load "/Users/dsletten/lisp/packages/lang.lisp")
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :dpsq2 (:use :common-lisp :lang :test))

(in-package :dpsq2)

(defun isa-get (node property)
  (cond ((null node) nil)
        ((get node property))
        (t (isa-get (get node 'isa) property))))

;;;
;;;    Pretty-print property list
;;;    
(defun ppp (symbol)
  (format t "~A~%" (symbol-name symbol))
  (dotuples ((property value) (symbol-plist symbol))
            (format t "~4@T~S~20T~S~%" property value)))

(defmacro pppq (symbol)
  `(ppp ',symbol))

(defun dps (node &rest plist)
  (dotuples ((property value) plist)
    (let ((fn (isa-get property '+lambda-property))) ; Why ISA-GET????
      (if fn
          (funcall fn node property value)
          (ddput node property value)))) )

(defmacro dpsq (&rest args)
  `(apply #'dps ',args))

;;;
;;;    +INVERT-VALUE implies that NODE and VALUE are of the same category of things??
;;;    
(defun ddput (node property value &optional recursive-p)
  (let ((onto (isa-get property '+invert-onto)))
    (when onto
      (ddput value onto node)))
  (when (get property '+invert-property) ; Only execute if property directly has +invert-property
    (ddput property value node)) ; No list for singleton???
  (cond ((isa-get property '+multiple-values)
         (set-property node property value :multi-valued-p t) ; This has to be set first before we look at inverting!!
         (when (isa-get property '+invert-value)
           (invert-value node property value recursive-p)
           (dolist (related-node (get-existing-values node property))
             (unless (or (eql related-node value)
                         (member value (get-existing-values related-node property)))
               (ddput related-node property value))))) ; Transitivity
        ((isa-get property '+save-property)
         (let ((current-value (get node property)))
           (cond ((null current-value)
                  (set-property node property value)
                  (when (isa-get property '+invert-value)
                    (invert-value node property value recursive-p)))
                 ((not (eql current-value value))
                  (save-property node property current-value)
                  (set-property node property value)
                  (when (isa-get property '+invert-value) ; Remove from current node
                    (save-property current-value property node)
                    (remprop current-value property)
                    (invert-value node property value recursive-p)))) ))
        (t (set-property node property value)
           (when (isa-get property '+invert-value) ; None of Slade's examples get here...
             (invert-value node property value recursive-p)))) )

(defun invert-value (node property value recursive-p)
  (unless recursive-p
    (ddput value property node t)))

;;;
;;;    Relies on caller to remove/update old value...
;;;    
(defun save-property (node property old-value)
  (let ((save-node (gensym "SAVE")))
    (set-property save-node property old-value)
    (set-property node 'save-node save-node)))

(defun set-property (node property value &key multi-valued-p)
  (if multi-valued-p
      (setf (get node property) (add-to-multi-valued-property node property value))
      (setf (get node property) value)))

;;;
;;;    Returns existing value(s) as a (possibly empty!) list. Single value wrapped as single-elt list.
;;;    
(defun get-existing-values (node property)
  (let ((values (get node property)))
    (if (listp values)
        values
        (list values))))

;;;
;;;    Add a new VALUE to the PROPERTY on NODE.
;;;    If the PROPERTY does not yet have a value, then VALUE becomes the value.
;;;    If the PROPERTY already has a value and it is the same as VALUE, ignore VALUE.
;;;    If the PROPERTY has a single value distinct from VALUE set the list containing both as the new value.
;;;    If the PROPERTY already has a list of values, and VALUE to the list if not already present.
;;;    (This function merely produces the appropriate value. SET-PROPERTY actually sets it on NODE.)
;;;    
(defun add-to-multi-valued-property (node property value)
  (let ((current (get node property)))
    (cond ((null current) value)
          ((eql current value) current)
          ((listp current) (adjoin value current))
          (t (list value current)))) )

(defun load-db (db)
  (dolist (entry db)
    (apply #'dps entry)))

(defvar *db* '((jane isa programmer sex female income 60k)
               (john isa programmer sex male ingests junk-food)
               (programmer isa person income 50k)
               (person isa mammal)
               (mammal isa organism)
               (organism ingests (food air))))

(defvar *db2* '((isa +invert-onto instances) ; +multiple-values???
                (instances +multiple-values t) ; Instances never used operationally. Just a record...
                (*relationship +multiple-values t)
                (relationship +save-property t)
                (children isa *relationship)
                (sibling isa *relationship +invert-value t) ; Broadest sense. Includes half/step-siblings. Not strictly correct. Half siblings of same sibling not necessarily siblings.
;                (parent +lambda-property #'(lambda (parent relation child) (declare (ignore relation)) (dps parent 'children child)))
;                (father isa parent)
                (father isa relationship)
                (spouse isa relationship +invert-value t)
                (joe-jr father joe-sr spouse mary children pat children sue)))

(defvar *db3* '((mary sibling dorothy sibling arthur)))
(defvar *db4* '((joe-jr spouse louise children jackie)))
(defvar *db5* '((job +invert-property t +multiple-values t)
                (plumber isa job)
                (carpenter isa job)
                (joe-jr job plumber job carpenter)))

(dps 'son '+lambda-property #'(lambda (parent relation son) 
                                (declare (ignore relation)) ; ?!?!?!?
                                (let ((siblings (get-existing-values parent 'children)))
                                  (unless (null siblings)
                                    (dolist (sibling siblings)
                                      (dps son 'sibling sibling))))
                                (dps parent 'children son)
                                (dps son 'sex 'male 'parent parent)))
(dps 'daughter '+lambda-property #'(lambda (parent relation daughter) 
                                     (declare (ignore relation)) ; ?!?!?!?
                                     (let ((siblings (get-existing-values parent 'children)))
                                       (unless (null siblings)
                                         (dolist (sibling siblings)
                                           (dps daughter 'sibling sibling))))
                                     (dps parent 'children daughter)
                                     (dps daughter 'sex 'female 'parent parent)))

;;;
;;;    This isn't quite right???
;;;    
;; (defvar *db6* `((dps 'son '+lambda-property ,#'(lambda (node prop val) (ddput node 'children val) (ddput val 'sex 'male) (ddput val 'father node)))
;;                 (dpsq joe-jr son lester)
;;                 (dpsq joe-sr son joe-jr)))

(let ((brother1 (gensym))
      (brother2 (gensym))
      (brother3 (gensym)))
  (dps brother1 'sibling brother2)
  (ppp brother1)
  (ppp brother2)
  (dps brother1 'sibling brother3)
  (ppp brother1)
  (ppp brother2)
  (ppp brother3))

(let ((brother1 (gensym))
      (brother2 (gensym))
      (brother3 (gensym))
      (brother4 (gensym)))
  (dps brother1 'sibling brother2)
  (ppp brother1)
  (ppp brother2)
  (dps brother3 'sibling brother4)
  (ppp brother3)
  (ppp brother4)
  (dps brother1 'sibling brother3)
  (ppp brother1)
  (ppp brother2)
  (ppp brother3)
  (ppp brother4))

(let ((parent (gensym))
      (son (gensym)))
  (dps parent 'son son)
  (ppp parent)
  (ppp son))

(let ((parent (gensym))
      (daughter (gensym)))
  (dps parent 'daughter daughter)
  (ppp parent)
  (ppp daughter))

  
