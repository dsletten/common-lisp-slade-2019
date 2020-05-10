;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               test-cl-eval.lisp
;;;;
;;;;   Started:            Tue Apr 14 20:11:06 2020
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
(load "/Users/dsletten/lisp/books/Slade/2019/ch15/cl-eval.lisp")

(in-package :cl-eval)

(deftest test-cl-eval ()
  (check
   ;; (= (cl-eval '((lambda (x) (+ x 1)) 9)) 10)
   ;; (= (cl-eval '((lambda (x) (+ x y)) 9) '((y . 3))) 12)
   ;; (= (cl-eval '((lambda (x) (f x 1)) 9) '((f . (lambda (x y) (+ x y)))) ) 10)
   (equal (cl-eval 9) 9)
   (equal (cl-eval "pung") "pung")
   (equal (cl-eval #\a) #\a)
   (equal (cl-eval t) t)
   (equal (cl-eval nil) nil)
   (equal (cl-eval pi) pi)
   (equal (cl-eval :t) :t)
   (equal (cl-eval '(quote pung)) 'pung)
   (equal (cl-eval '(function car)) #'car)
   ;; (cl-eval '(and (< 3 4) (= (+ x 2) 5)) '((x . 3)))
   ;; (not (cl-eval '(and (< 4 3) (= (+ x 2) 5)) '((x . 3))))
   ;; (cl-eval '(or (< 4 3) (= (+ x 2) 5)) '((x . 3)))
   ;; (not (cl-eval '(or (< 4 3) (= (+ x 2) 5)) '((x . 8))))
   (equal (cl-eval '(car '(a b c))) 'a)
   (equal (cl-eval '(cdr '(a b c))) '(b c))
   (equal (cl-eval '(cons (car '(a b c)) (cdr '(a b c)))) '(a b c))
   (equal (cl-eval '(list 1 2 3)) '(1 2 3))
   (equal (cl-eval '(list)) '())
   (equal (cl-eval '(cons (cons 'a 'b) (cons 'c 'd))) '((a . b) . (c . d)))
   (equal (cl-eval '(cons (list 'a 'b) (list 'c 'd))) '((a b) c d))
   (equal (cl-eval '(list (cons 'a 'b) (cons 'c 'd))) '((a . b) (c . d)))
   ;; (not (cl-eval '(zerop x) '((x . 5) (zerop . (lambda (x) (eq x 0)))) ))
   ;; (cl-eval '(zerop x) '((x . 0) (zerop . (lambda (x) (eq x 0)))) )
   (cl-eval '(atom 99))
   (cl-eval '(atom #(1 2 3)))
   (cl-eval '(atom "Is this not pung?"))
   (cl-eval '(eq 'pung 'pung))
   (not (cl-eval '(eq '(pung) '(pung))))
   (cl-eval '(equal '(pung) '(pung)))
   (cl-eval '(equalp "Is this not pung?" "IS THIS NOT PUNG?"))
   ;; (equal (cl-eval '(cond ((zerop x) (print 'pung) (print (eq 5 3)) (car '(a b c))) (t 'foo)) '((x . 0) (zerop . (lambda (x) (eq x 0)))) ) 'a)
   ;; (equal (cl-eval '(cond ((zerop x) (print 'pung) (print (eq 5 3)) (car '(a b c))) (t 'foo)) '((x . 1) (zerop . (lambda (x) (eq x 0)))) ) 'foo)
   ;; (equal (cl-eval '(cond ((zerop x) (print 'pung) (print (eq 5 3)) (car '(a b c)))) '((x . 1) (zerop . (lambda (x) (eq x 0)))) ) nil)
   (equal (cl-eval '(cond ((< 3 3)) (t :foo))) :FOO)
   (equal (cl-eval '(cond ((< 2 3)) (t :foo))) T)
   (equal (cl-eval '(cond ((< 2 3) :bar) (t :foo))) :BAR)
   (equal (cl-eval '(cond ((< 3 3) :bar))) NIL)



   ;; (equal (cl-eval '(if (zerop x) (cons x '()) z) '((x . 0) (z . 9) (zerop . (lambda (x) (eq x 0)))) ) '(0))
   ;; (equal (cl-eval '(if (zerop x) (cons x '()) z) '((x . 1) (z . 9) (zerop . (lambda (x) (eq x 0)))) ) 9)
   ;; (cl-eval '(f (car '(0 1 2))) '((f . (lambda (x) (zerop x))) (zerop . (lambda (y) (eq y 0)))) )
   ;; (not (cl-eval '(f (car '(3 0 1 2))) '((f . (lambda (x) (zerop x))) (zerop . (lambda (y) (eq y 0)))) ))
   ;; (= (cl-eval '(+)) addition-identity)
   (= (cl-eval '(+ . #1=(2))) (cl:+ . #1#))
   (= (cl-eval '(+ . #2=(2 3))) (cl:+ . #2#))
   (= (cl-eval '(+ . #3=(2 3 4))) (cl:+ . #3#))
   (= (cl-eval '(+ . #4=(5 2 3 4))) (cl:+ . #4#))
   ;; (= (cl-eval '(*)) multiplication-identity)
   (= (cl-eval '(* . #5=(2))) (cl:* . #5#))
   (= (cl-eval '(* . #6=(2 3))) (cl:* . #6#))
   (= (cl-eval '(* . #7=(2 3 4))) (cl:* . #7#))
   (= (cl-eval '(* . #8=(5 2 3 4))) (cl:* . #8#))
   ;; (= (cl-eval '(+ (* a x x) (* b x) c) '((a . 3) (b . 4) (c . 9) (x . 10))) 349) ; Base 10 #10r349
   ;; (= (cl-eval '(+ (* a x x) (* b x) c) '((a . 3) (b . 4) (c . 9) (x . 16))) 841) ; Hex #x349
   ;; (= (cl-eval '(/ (- (* x x) 9) (+ x 3)) '((x . 6))) 3) ; (x^2 - 9)/(x + 3) => (x - 3)
   ;; (= (cl-eval '(/ (- (* x x) 16) (- x 4)) '((x . 8))) 12)
   (cl-eval '(= 8))
   (cl-eval '(= 3 3.0))
   (not (cl-eval '(= 3 3.1)))
   (cl-eval '(= (+ 1 4) (/ 10 2) (- 12 7) (* 5/9 9)))
   (cl-eval '(/= 8))
   (cl-eval '(/= 3 3.1))
   (not (cl-eval '(/= 3 3.0)))
   (cl-eval '(/= 2 3 4 5))
   (not (cl-eval '(/= 2 3 4 3)))
   (cl-eval '(< 2))
   (cl-eval '(< 2 8))
   (not (cl-eval '(< 2 2)))
   (not (cl-eval '(< 8 2)))
   (cl-eval '(< 2 8 9))
   (not (cl-eval '(< 2 8 8)))
   (cl-eval '(<= 2))
   (cl-eval '(<= 2 8))
   (cl-eval '(<= 2 2))
   (not (cl-eval '(<= 8 2)))
   (cl-eval '(<= 2 8 9))
   (cl-eval '(<= 2 8 8))
   (not (cl-eval '(<= 2 8 3)))
   (cl-eval '(> 2))
   (cl-eval '(> 8 2))
   (not (cl-eval '(> 2 2)))
   (not (cl-eval '(> 2 8)))
   (cl-eval '(> 9 8 2))
   (not (cl-eval '(> 8 2 2)))
   (cl-eval '(>= 2))
   (cl-eval '(>= 8 2))
   (cl-eval '(>= 2 2))
   (not (cl-eval '(>= 2 8)))
   (cl-eval '(>= 9 8 2))
   (cl-eval '(>= 8 2 2))
   (not (cl-eval '(>= 8 2 7)))
   (eq (cl-eval '(let ((x 8) (y 5)) (if (> x y) :foo :bar))) :FOO)
   (eq (cl-eval '(let ((x 8) (y 8)) (if (> x y) :foo :bar))) :BAR)
   (cl-eval '(let ((x (- 2 y)) (z x)) (+ x z)) (make-local-binding 'y 12 (make-binding 'x 2.6 nil)))
   (cl-eval '(let* ((x (- 2 y)) (z x)) (+ x z)) (make-binding 'y 12 nil))
   ;; (let ((l (loop for i from 1 to 10 collect i)))
   ;;   (notany #'null (loop for i from 1 to 10 ; FIRST ... TENTH
   ;;                        for f = (intern (string-upcase (format nil "~:R" i)))
   ;;                        collect (eq (cl-eval (list f (list 'quote l)))
   ;;                                    (funcall f l)))) )))
))


;; * (cl-eval '(append '(a b c) '(d e)))
;; (A B C D E)
;; * (cl-eval '(append '(a b c) 'd))
;; (A B C . D)

;; * (cl-eval (read))
;; (subst 'foo 'pung '(is this not pung))
;; (IS THIS NOT FOO)
;; * (subst 'foo 'pung '((pung) (bar (pung)) pung pung ((((pung))))))
;; ((FOO) (BAR (FOO)) FOO FOO ((((FOO)))))
;; * (cl-eval (read))
;; (subst 'foo 'pung '(is this not pung))
;; (IS THIS NOT FOO)
;; * (cl-eval (read))
;; (subst 'foo 'pung '((pung) (bar (pung)) pung pung ((((pung))))))
;; ((FOO) (BAR (FOO)) FOO FOO ((((FOO)))))
;; * (trace cl-eval)
;; (CL-EVAL)
;; * (cl-eval (read))
;; (subst 'foo 'pung '((pung) (bar (pung)) pung pung ((((pung))))))


;; * (cl-eval (read))
;; (member 'a '(4 9 a c d))
;; (A C D)
;; * (member 'a '(4 9 (a c) d))
;; NIL
;; * (member 'd '(4 9 a c d))
;; (D)
;; * (cl-eval (read))
;; (member 'a '(4 9 (a c) d))
;; NIL
;; * (cl-eval (read))
;; (member 'd '(4 9 a c d))
;; (D)
;; *

;; * (cl-eval (read))
;; (assoc 'f '((a . 1) (b . 2) (c . 3) (d . e)))
;; NIL
;; * (assoc 'f '((a . 1) (b . 2) (c . 3) (d . e)))
;; NIL
;; * (cl-eval (read))
;; (assoc 'c '((a . 1) (b . 2) (c . 3) (d . e)))
;; (C . 3)
;; * (assoc 'c '((a . 1) (b . 2) (c . 3) (d . e)))
;; (C . 3)

;; * (cl-eval (read))
;; (reverse '(a b c d))
;; (D C B A)
;; * (reverse '(a b c d))
;; (D C B A)
;; * (cl-eval (read))
;; (reverse '())
;; NIL
;; * (reverse '())
;; NIL
;; * (cl-eval (read))
;; (reverse '(a (b c) d e))
;; (E D (B C) A)
;; * (reverse '(a (b c) d e))
;; (E D (B C) A)


;; (cl-eval (read))
;; (funcall #'list 1 2 3)
;; (1 2 3)
;; * (cl-eval (read))
;; (reduce #'+ '(1 2 3))
;; 6
;; * (cl-eval (read))
;; (mapcar #'(lambda (x) (* x 5)) '(2 4 6))
;; (10 20 30)
;; * 

(defvar *e1* (make-binding nil nil nil))
(defvar *e2* (make-binding 'x 20 *e1*))
(defvar *e3* (make-local-binding 'y 12 *e2*))
(defvar *e4* (make-local-binding 'z -3 *e1*))
(defvar *e5* (make-binding 'p 1.2 *e1*))



;; (cl-eval '(and) nil)
;; T
;; * (cl-eval '(and (< 2 3)) nil)
;; T
;; * (cl-eval '(and (< 2 3) (> 9 8)) nil)
;; T
;; * (cl-eval '(and (< 2 3) (> 9 8) (truncate 8 3)) nil)
;; 2
;; 2
;; (cl-eval '(and (< 2 3) (> 9 8) nil (truncate 8 3)) nil)
;; NIL

;; (cl-eval '(or) nil)
;; NIL
;; * (cl-eval '(or (> 2 3)) nil)
;; NIL
;; * (cl-eval '(or (< 2 3)) nil)
;; T
;; (cl-eval '(or (< 3 2) (> 9 8)) nil)
;; T
;; * (cl-eval '(or (< 3 2) (> 8 9) (truncate 8 3)) nil)
;; 2
;; 2
;; * (cl-eval '(or (< 3 2) (> 8 9) (truncate 8 3) :pung) nil)
;; 2
;; * 

;;;
;;;    Slade's test examples (pg. 634-635)
;;;    
;; (defvar *e* (make-binding nil nil nil))
;; (cl-eval '(cond (nil 1 2) (t 3 4)) *e3*)

;; 4
;; (cl-eval '(case 2 ((1) 'one) ((2) 'two) (otherwise 'many)) *e*)

;; TWO
;; (cl-eval '(case 3 ((1) 'one) ((2) 'two) (otherwise 'many)) *e*)

;; MANY
;; (cl-eval '(or (setq x 1) (setq x 2)) *e*)

;; 1
;; (cl-eval 'x *e*)

;; 1

;; (cl-eval '(and (setq x 1) (setq x 2)) *e*)

;; 2
;; (cl-eval 'x *e*)

;; 2

;; (cl-eval '(let ((a 1) (b 2)) (+ a b)) *e*)

;; 3
