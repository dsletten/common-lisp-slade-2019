;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch16.lisp
;;;;
;;;;   Started:            Sat Dec 28 16:40:16 2019
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
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :ch16 (:use :common-lisp :test) (:shadow :reverse :append :last :nth :remove-duplicates :boole))

(in-package :ch16)

;;;
;;;    16.8.3
;;;    Tail recursion is natural way to write this! Consing from head makes it come out backwards anyway.
;;;    
(defun reverse (l)
  (labels ((reverse-aux (l result)
	     (cond ((endp l) result)
		   (t (reverse-aux (rest l) (cons (first l) result)))) ))
    (reverse-aux l '())))

(deftest test-reverse ()
  (check
   (equal (reverse #1='()) (cl:reverse #1#))
   (equal (reverse #2='(a)) (cl:reverse #2#))
   (equal (reverse #3='(a b)) (cl:reverse #3#))
   (equal (reverse #4='(a b c d e)) (cl:reverse #4#))
   (equal (reverse #5='((a 1) (b 2) (c 3) (d 4))) (cl:reverse #5#))))

;;;
;;;    16.8.4
;;;    
(defun append (l m)
  (cond ((endp l) m)
	(t (cons (first l) (append (rest l) m)))) )

;;;
;;;    Weird...reverses first arg.
;;;    See ~/lisp/programs/implementation/append.lisp MY-APPEND-2 (Although it reverses first arg for different reason!)
;;;    
(defun append (l m)
  (labels ((append-aux (m result)
	     (cond ((endp m) (nreverse result))
		   (t (append-aux (rest m) (cons (first m) result)))) ))
    (append-aux m (reverse l))))
   
(deftest test-append ()
  (check
   (equal (append #1='() #2='(x y)) (cl:append #1# #2#))
   (equal (append #3='(a) #2#) (cl:append #3# #2#))
   (equal (append #4='(a b c) #2#) (cl:append #4# #2#))))

;;;
;;;    16.8.5
;;;    
(defun last (l)
  (cond ((atom (cdr l)) l)
	(t (last (cdr l)))) )

(deftest test-last ()
  (check
   (equal (last '(1)) '(1))
   (equal (last '(a b . c)) '(b . c))
   (equal (last '(a b c d)) '(d))))

;;;
;;;    16.8.6
;;;    
(defun nth (n list)
  (if (zerop n)
      (first list)
      (nth (1- n) (rest list))))

(deftest test-nth ()
  (check
   (equal (nth 0 '(a b c)) 'a)
   (equal (nth 2 '(a b c)) 'c)
   (equal (nth 4 '(a b c)) nil)))

;;;
;;;    16.8.7
;;;    
(defun remove-duplicates (list)
  (cond ((endp list) '())
	((member (first list) (rest list)) (remove-duplicates (rest list)))
	(t (cons (first list) (remove-duplicates (rest list)))) ))

(defun remove-duplicates (list)
  (labels ((remove-duplicates-aux (l result)
	     (cond ((endp l) (nreverse result))
		   ((member (first l) (rest l)) (remove-duplicates-aux (rest l) result))
		   (t (remove-duplicates-aux (rest l) (cons (first l) result)))) ))
    (remove-duplicates-aux list '())))

(deftest test-remove-duplicates ()
  (check
   (equal (remove-duplicates '()) '())
   (equal (remove-duplicates '(a)) '(a))
   (equal (remove-duplicates '(a a)) '(a))
   (equal (remove-duplicates '(a b a)) '(b a))
   (equal (remove-duplicates '(a b c a b c)) '(a b c))
   (equal (remove-duplicates '(a b c d a b c)) '(d a b c))))

;;;
;;;    16.8.8
;;;    
(defvar *boole-functions* (let ((fns (make-array 16))
				(function-list (list (list boole-1 #'(lambda (int-1 int-2)
								       (declare (ignore int-2))
								       int-1))
						     (list boole-2 #'(lambda (int-1 int-2)
								       (declare (ignore int-1))
								       int-2))
						     (list boole-andc1 #'(lambda (int-1 int-2)
									   (logandc1 int-1 int-2)))
						     (list boole-andc2 #'(lambda (int-1 int-2)
									   (logandc2 int-1 int-2)))
						     (list boole-and #'(lambda (int-1 int-2)
									 (logand int-1 int-2)))
						     (list boole-c1 #'(lambda (int-1 int-2)
									(declare (ignore int-2))
									(lognot int-1)))
						     (list boole-c2 #'(lambda (int-1 int-2)
									(declare (ignore int-1))
									(lognot int-2)))
						     (list boole-clr #'(lambda (int-1 int-2)
									 (declare (ignore int-2))
									 (logxor int-1 int-1))) ;;;
						     (list boole-eqv #'(lambda (int-1 int-2)
									 (logeqv int-1 int-2)))
						     (list boole-ior #'(lambda (int-1 int-2)
									 (logior int-1 int-2)))
						     (list boole-nand #'(lambda (int-1 int-2)
									 (lognand int-1 int-2)))
						     (list boole-nor #'(lambda (int-1 int-2)
									 (lognor int-1 int-2)))
						     (list boole-orc1 #'(lambda (int-1 int-2)
									   (logorc1 int-1 int-2)))
						     (list boole-orc2 #'(lambda (int-1 int-2)
									   (logorc2 int-1 int-2)))
						     (list boole-set #'(lambda (int-1 int-2)
									 (declare (ignore int-2))
									 (lognot (logxor int-1 int-1))))
						     (list boole-xor #'(lambda (int-1 int-2)
									 (logxor int-1 int-2)))) ))
			    (loop for (index f) in function-list
				  do (setf (aref fns index) f))
			    fns))
			      
(defun boole (op int-1 int-2)
  (funcall (aref *boole-functions* op) int-1 int-2))

(deftest test-boole ()
  (check
   (every #'(lambda (op) (= (boole op 2 -8) (cl:boole op 2 -8)))
	  (list BOOLE-XOR BOOLE-CLR BOOLE-2 BOOLE-EQV BOOLE-NAND BOOLE-IOR BOOLE-ANDC1
	  	BOOLE-C1 BOOLE-SET BOOLE-ORC2 BOOLE-ANDC2 BOOLE-NOR BOOLE-C2
	  	BOOLE-ORC1 BOOLE-AND BOOLE-1))))

