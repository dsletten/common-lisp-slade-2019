;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp isn't a language, it's a building material.
;;;;   -- Alan Kay
;;;;
;;;;   Name:               ch11.lisp
;;;;
;;;;   Started:            Sun Sep 29 00:14:27 2019
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

(defpackage :ch11 (:use :common-lisp :test) (:shadow :prog1))

(in-package :ch11)

;;;
;;;    11.10.2
;;;
(defmacro swap (a b)
  `(psetf ,a ,b ,b ,a))

;;;
;;;    11.10.3, 11.10.4
;;;
(defmacro slade-repeat (n &body body)
  `(do ((count ,n (- count 1)))
       ((<= count 0) nil)
     ,@body))

(defmacro repeat-do (n &body body)
  (let ((counter (gensym)))
    `(do ((,counter ,n (1- ,counter)))
         ((<= ,counter 0) nil)
       ,@body)))

(defmacro repeat-dotimes (n &body body)
  (let ((i (gensym)))
    `(dotimes (,i ,n)
;       (declare (ignore ,i))    <-- No! DOTIMES expands to use the variable!
       ,@body)))

(defmacro repeat-simple-loop (n &body body)
  (let ((counter (gensym)))
    `(let ((,counter ,n))
       (loop (when (<= ,counter 0) (return))
          ,@body
          (decf ,counter)))) )

(defmacro repeat-loop (n &body body)
  `(loop repeat ,n
         do ,@body))

(defmacro repeat-tagbody (n &body body)
  (let ((start-tag (gensym))
        (counter (gensym)))
    `(block nil
       (let ((,counter ,n))
         (tagbody
            ,start-tag
            (when (<= ,counter 0) (return))
            ,@body
            (decf ,counter)
            (go ,start-tag)))) ))

;;;
;;;    11.10.5
;;;
;;;    See Graham ACL pg. 164
;;;    
(defmacro while-do (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro while-simple-loop (test &body body)
  `(loop (unless ,test (return))
      ,@body))

(defmacro while-loop (test &body body)
  `(loop while ,test
         do ,@body))

(defmacro while-tagbody (test &body body)
  (let ((tag (gensym)))
    `(block nil
       (tagbody
          ,tag
          (unless ,test (return))
          ,@body
          (go ,tag)))) )

;;;
;;;    11.10.6
;;;
;;;    Simple retread of WHILE macro above.
;;;    
(defmacro until-do (test &body body)
  `(do ()
       (,test)
     ,@body))

(defmacro until-simple-loop (test &body body)
  `(loop (when ,test (return))
      ,@body))

(defmacro until-loop (test &body body)
  `(loop until ,test
         do ,@body))

(defmacro until-tagbody (test &body body)
  (let ((tag (gensym)))
    `(block nil
       (tagbody
          ,tag
          (when ,test (return))
          ,@body
          (go ,tag)))) )

;;;
;;;    11.10.7
;;;
(defmacro bad-prog1 (first &body body)
  `((lambda (x) ,@body x) ,first))

(defmacro prog1 (first &body body)
  (let ((result (gensym)))
    `((lambda (,result) ,@body ,result) ,first)))

;;;
;;;    11.10.8
;;;
;;;
;;;    Slade quotes the whole damn thing:
;;;
;; (defun dps (&rest l)
;;   (let ((node (car l)))
;;     (do ((l (cdr l) (cddr l)))
;;         ((null l) nil)
;;       (setf (get node (car l)) (cadr l)))) )
;; ;      (put node (car l) (cadr l)))) ) <-- He refers to archaic PUT

;; ;;;
;; ;;;    My translation...
;; ;;;    
;; (defun dps (&rest l)
;;   (do ((node (first l))
;;        (plist (rest l) (rest (rest plist))))
;;       ((endp plist) nil)
;;     (setf (get node (first plist)) (second plist))))

;; (defmacro dpsq (&body l)
;;   `(apply #'dps (quote ,l)))

;; (defmacro dpsq (&body l)
;;   `(apply #'dps  ',l))

(defun dps (symbol &rest props)
  (loop for (property value) on props by #'cddr
        do (setf (get symbol property) value)))

(defmacro dpsq (symbol &rest props)
  `(dps ',symbol ,@(mapcar #'(lambda (elt) `',elt) props)))

;;;
;;;    11.10.9
;;;
;;;
;;;    Slade's version uses an interesting short-hand, but he doesn't follow through.
;;;
;; (defun isa-get (node property)
;;   (cond ((get node property))
;;         ((get node 'isa)
;;          (isa-get (get node 'isa) property))
;;         (t nil)))

;; (defun isa-get (node property)
;;   (cond ((null node) nil)
;;         ((get node property))
;;         ((isa-get (get node 'isa) property))
;;         (t nil)))

(defun isa-get (node property)
  (let ((value (get node property)))
    (if (null value)
        (let ((link (get node 'isa)))
          (if (null link)
              nil
              (isa-get link property)))
        value)))

(unless (boundp '*hierarchy*)
  (defvar *hierarchy* '((jane isa programmer sex female income sixy-k)
                        (john isa programmer sex male ingests junk-food)
                        (programmer isa person income fifty-k)
                        (person isa mammal)
                        (mammal isa organism)
                        (organism ingests (food air))))
  
  (dolist (node *hierarchy*)
    (apply #'dps node)))

;;;
;;;    11.10.10
;;;
;;;    See dpsq.lisp
;;;

;;;
;;;    11.10.11
;;;
(defmacro msg (&body body)
  (let ((forms (mapcar #'resolve body)))
    `(progn ,@forms)))

(defun resolve (arg)
  (typecase arg
    (boolean `(terpri)) ; NIL???
    (symbol `(format t "~A" ,arg))
    (string `(write-string ,arg))
    (number (if (minusp arg)
                `(format t "~v%" ,(abs arg)) ; Backquote vs. double-quote????
                `(format t "~A" (make-string ,arg :initial-element #\space))))
    (cons (case (first arg)
            (hex `(format t "~X" ,(second arg)))
            (oct `(format t "~O" ,(second arg)))
            (bin `(format t "~B" ,(second arg)))
            (plur `(format t "~P" ,(second arg)))) )))

;(let ((*standard-output* stream)) ; Must collect args until next (to stream) arg!

;;;
;;;    From 2012
;;;    
#|
(msg "Hello" t)
Hello
NIL
CH11(189): (msg "Hello" t "there" t)
Hello
there
NIL
CH11(190): (let ((x 5)) (msg "John is" 1 x 1 "year" (plur x) 1 "old" t))
John is 5 years old
NIL
CH11(191): (let ((x 5)) (msg (bin x) " + " (bin x) " = " (bin (+ x x)) t))
101 + 101 = 1010
NIL
|#


;;;
;;;    11.10.12
;;;    
(set-macro-character #\! #'(lambda (stream ch)
                             (declare (ignore ch))
                             (loop for char = (read-char stream t nil t)
                                   until (char= char #\()
                                   finally (unread-char char stream))))

;;;
;;;    11.10.13
;;;    
(defmacro negatef (n)
  `(setf ,n (- ,n)))

(define-modify-macro negatef () -)

(defmacro invertf (n)
  `(setf ,n (/ ,n)))

(define-modify-macro invertf () /)

(defmacro upcasef (s)
  `(setf ,s (string-upcase ,s)))

;; (let ((i 3) (s "Is this not pung?")) (upcasef (subseq s (incf i) 7)) s)
;; "Is tISs not pung?"

(define-modify-macro upcasef () string-upcase)

(defmacro downcasef (s)
  `(setf ,s (string-downcase ,s)))

;; (let ((i 3) (s "Is THIS not pung?")) (downcasef (subseq s (incf i) 7)) s)
;; "Is TisS not pung?"

(define-modify-macro downcasef () string-downcase)
