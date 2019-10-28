;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a language for doing what you've been told is impossible.
;;;;   -- Kent Pitman
;;;;
;;;;   Name:               dpsq-slade.lisp
;;;;
;;;;   Started:            Mon Oct 14 20:51:30 2019
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
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :dpsq-slade (:use :common-lisp :test) (:shadow :get-properties))

(in-package :dpsq-slade)

;;;
;;;    Define PropertieS
;;;    
(defun dps (node &rest plist)
  (loop for (property value) on plist by #'cddr
        do (ddput-slade node property value)))

;;;
;;;    Define PropertieS-Quoted...Slade-style
;;;    
(defmacro dpsq (node &rest plist)
  `(apply #'dps ',node ',plist))

(defun isa-get (node property)
  (cond ((null node) nil)
        ((get node property))
        ((isa-get (get node 'isa) property))))

;;;
;;;    Slade's mess
;;;
;; (defun ddput-slade (node prop val)
;;   (and (symbolp node)
;;        (symbolp prop)
;;        (progn (cond ((isa-get prop '+invert-property) (ddput-slade prop val node)))
;;               (cond ((isa-get prop '+invert-value) (*put val prop node)))
;;               (let ((onto (isa-get prop '+invert-onto)))
;;                 (if onto (ddput-slade val onto node)))
;;               (cond ((isa-get prop '+multiple-values) (add-property node prop val))
;;                     ((and (get node prop)
;;                           (isa-get prop '+save-property))
;;                      (*put (or (get node 'save-node)
;;                                (ddput-slade node 'save-node (gensym "SAVE")))
;;                            prop
;;                            (get node prop))
;;                      (*put node prop val))
;;                     (t (let ((fn (isa-get prop '+lambda-property)))
;;                          (if fn
;;                              (apply fn (list node prop val))
;;                              (*put node prop val)))) )
;;               val)))

;;;
;;;    1. Should never get into an infinite loop. As defined by Slade, only JOB has
;;;       an +INVERT-PROPERTY property. Furthermore, CARPENTER and PLUMBER "inherit" the
;;;       property via ISA relation. Thus an initial call:
;;;       (DDPUT-SLADE 'JOE-JR 'JOB 'PLUMBER)
;;;       causes a 2nd call on JOB due to +INVERT-ONTO:
;;;       (DDPUT-SLADE 'JOB 'PLUMBER 'JOE-JR)
;;;       This in turn triggers a 3rd call due to ISA +INVERT-ONTO:
;;;       (DDPUT-SLADE 'PLUMBER 'JOE-JR 'JOB)
;;;       But the cycle has to stop there since the original NODE becomes the PROP here and
;;;       won't have an +INVERT-ONTO property itself. (Maybe some kind of meta-property as
;;;       initial NODE??)
;;;       
;;;       Result:
;;;       JOE-JR
;;;           JOB             (CARPENTER PLUMBER)
;;;           SAVE-NODE       #:SAVE20371
;;;           CHILDREN        (JACKIE SUE PAT)
;;;           SPOUSE          LOUISE
;;;           FATHER          JOE-SR
;;;       JOB
;;;           CARPENTER       (JOE-JR)
;;;           PLUMBER         (JOE-JR)
;;;           INSTANCES       (CARPENTER PLUMBER)
;;;           +MULTIPLE-VALUES T
;;;           +INVERT-PROPERTY T
;;;       PLUMBER
;;;           JOE-JR          JOB    ; Does this make any sense???? (Faulty use of inheritance? (ISA))
;;;           ISA             JOB
;;;
;;;       (Inheritance doesn't really work here anyway...The concrete JOBs are never properties on people
;;;        but rather merely values. Calling (DDPUT-SLADE 'JOE-JR 'PLUMBER ...) doesn't make any sense.
;;;        PLUMBER and JOB are different kinds of things. JOB is more of a metaclass.)
;;;
;;;    DDPUT does not store any properties directly. It relies on *PUT to do that.
;;;    
(defun ddput-slade (node prop val)
  (when (isa-get prop '+invert-property)
    (ddput-slade prop val node)) ; 1. PROP becomes NODE. VAL becomes PROP.
  (when (isa-get prop '+invert-value) ; Symmetric relationship: SPOUSE, SIBLING.
                                      ; Broken...overwrites sibling without considering multiple values for "inverted" sibling!!
                                      ; I.e., +INVERT-VALUE and +MULTIPLE-VALUES don't play well together.
                                      ; Also asymmetric! (DDPUT A SIBLING B)
                                      ; A: SIBLING (B) <- +MULTIPLE-VALUES call ADD-PROPERTY
                                      ; B: SIBLING A   <- +INVERT-VALUE ...
    (*put val prop node)) ; This should be recursive call to DDPUT to avoid above problems (Must have RECURSIVE-P check!)
;  (let ((onto (isa-get prop '+invert-onto))) ; Class/instance relationship. This shouldn't call ISA-GET! Nothing ISA ISA!
  (let ((onto (get prop '+invert-onto)))
    (when onto
      (ddput-slade val onto node)))
  (cond ((isa-get prop '+multiple-values) (add-property node prop val))
        ((and (get node prop) ; Save property if one already present
              (isa-get prop '+save-property))
         (*put (or (get node 'save-node) ; Retrieve SAVE-NODE or create it
                   (ddput-slade node 'save-node (gensym "SAVE"))) ; Return value of DDPUT ... Just the GENSYM.
               prop
               (get node prop)) ; Save old value
         (*put node prop val)) ; Replace with new value
        (t (let ((fn (isa-get prop '+lambda-property)))
             (if fn
                 ;; (apply fn (list node prop val))   ?!?!?
                 (funcall fn node prop val)
                 (*put node prop val)))) ) ; Basic case to simply set a property on NODE! (Overwrites existing property)
  val)

;;;
;;;    Get the existing list of relevant properties saved on NODE.
;;;    
(defun get-properties (node)
  (get node '+ddprops))

;;;
;;;    Update the list of relevant properties saved on NODE.
;;;    
(defun (setf get-properties) (value node)
  (setf (get node '+ddprops) value))

;; (defun *put (node prop val)
;;   (cond (prop (setf (get node '+ddprops) (enter (get node '+ddprops) prop)) ; He uses +DDPROPS property on a symbol's plist to store all props for DPS.
;;               (setf (get node prop) val))))

;;;
;;;    Setting a property on a node also involves saving that property in the node's list of "Data-driven properties": +DDPROPS
;;;    
(defun *put (node prop val)
;  (when prop ; ????
  (setf (get-properties node) (enter (get-properties node) prop) ; Add new property key to +DDPROPS
        (get node prop) val)) ; Save the actual property value

;;;
;;;    Add a value to a property (must be multiple-valued property).
;;;    If property already exists as a single value, promote to list of values.
;;;    
(defun add-property (node prop val)
  (*put node prop (enter (get node prop) val)))

;;;
;;;    Handle promoting single value to list of multiple values.
;;;    
(defun enter (l value)
  (adjoin value (if (listp l) l (list l))))

;;;
;;;    Pretty-print properties
;;;    
(defun ppp (node)
  (let ((props (get-properties node)))
    (format t "~S~%" node)
    (loop for property in props
          for value = (get node property)
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
(trace ddput-slade)
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
                                               (ddput-slade node 'children value)
                                               (dps value 'sex 'male 'father node)))
                    (joe-jr son lester)))
    (dolist (node *son*)
      (apply #'dps node))))
