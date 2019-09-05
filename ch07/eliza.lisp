;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               eliza.lisp
;;;;
;;;;   Started:            Tue Aug 13 23:28:33 2019
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

(defpackage :eliza (:use :common-lisp :test))

(in-package :eliza)

;;;
;;;    4.7.12
;;;
(defun wild-card-p (obj)
  (eq obj '*wild*))
(defun matchp (pattern target)
  (cond ((endp pattern) (endp target))
        ((endp target) (and (wild-card-p (first pattern))
                            (matchp (rest pattern) target)))
        ((eq (first pattern) (first target))
         (matchp (rest pattern) (rest target)))
        ((wild-card-p (first pattern))
         (or (matchp (rest pattern) target)
             (matchp pattern (rest target))))
        (t nil)))

(defparameter *master-script* '(((*wild* laundry *wild*) "When my clothes get too dirty I just burn them.")
                                ((i am *wild*) "Do you think I care about that?")
                                ;((i want you *wild*) "Why do you want me *wild*?")
                                ((do you *wild*) "Why should you care about me?")
                                ((*wild* year *wild*) "If I'm lucky I'll graduate before the turn of the century.")
                                ((*wild* mother *wild*) "Don't make cracks about my mother. She's a saint.")
                                ((my name *wild*) "Glad to meet you. My friends call me Dr. Death.")
                                ((no *wild*) "Well pardon me for living.")
                                ((*wild* sick) "I think this room has lead paint. It makes you crazy.")
                                ((*wild*) "Really.")))

(defparameter *evangelisp-script* '(((*wild* language *wild*) "We must all learn to embrace Lisp.")
                                    ((i like *wild*) "There are many roads to Lisp.")
                                    ((do you *wild*) "This is not about me. The question is what can Lisp do for you?")
                                    ((how long *wild*) "How long did it take the masters to reach the peaks of Lisp excellence?")
                                    ((*wild* frustrated *wild*) "Mediate on Lisp. It will clear your mind.")
                                    ((*wild* seem *wild*) "Is not the list itself an illusion born of CONS?")
                                    ((*wild* parentheses *wild*) "Man does not live by CONS alone.")
                                    ((what is *wild*) "Ask the REPL.")
                                    ((*wild* lisp *wild*) "You are beginning to see the light.")
                                    ((*wild*) ("May your CONSes not be circular." "Recursion is your friend." "Do not fear the parentheses."))))

(defun hello ()
  (format t "Hi.~2%"))

(defun goodbye ()
  (format t "End of ELIZA.~%"))

(defun eliza (&optional (script *master-script*))
  (hello)
  (process-reply script)
  (goodbye))

(defconstant prompt "-->")
(defun process-reply (script)
  (format t "~A " prompt)
  (let ((reply (read)))
    (cond ((quitp reply) nil)
          ((not (listp reply)) (format t "~%*** Give input as a ( list ). Type Q to quit. ***~%")
           (process-reply script))
          (t (let ((match (script-match reply script)))
               (cond (match (fix-reply match)
                            (process-reply script))
                     (t nil)))) ))) ; Not possible when scipt contains catch-all (*wild*) pattern.

(defun quitp (reply)
  (member reply '(nil quit q)))

(defun script-match (input script)
  (if (endp script)
      nil
      (destructuring-bind (pattern response) (first script)
        (if (matchp pattern input)
            response
            (script-match input (rest script)))) ))

(defun fix-reply (response)
  (if (listp response)
      (fix-reply (choose-random response))
      (write-line response)))

(defun choose-random (responses)
  (nth (random (length responses)) responses))
