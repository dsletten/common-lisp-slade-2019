;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               test-simple-eval.lisp
;;;;
;;;;   Started:            Fri Mar 27 02:56:04 2020
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
(load "/home/slytobias/lisp/books/Slade/2019/ch15/simple-eval.lisp")

(in-package :simple-eval)

;; (deftest test-augment-environment ()
;;   (check
;;    (equal (augment-environment '(x y) '(2 5) '((x . 0))) '((x . 2) (y . 5) (x . 0)))
;;    (equal (augment-environment '(x y) '(2) '((x . 0))) '((x . 2) (y . nil) (x . 0)))) )

(deftest test-augment-environment ()
  (check
   (null (set-difference (augment-environment (pairlis '(x y) '(2 5)) '((x . 0))) '((x . 2) (y . 5) (x . 0)) :test #'equal))
   (null (set-difference (augment-environment (pairlis '(x y t pi) '(2 5 8 4)) '((x . 0))) '((x . 2) (y . 5) (x . 0)) :test #'equal))))
;   (equal (augment-environment (pairlis '(x y) '(2)) '((x . 0))) '((x . 2) (y . nil) (x . 0)))) )  ; Fix <---------------------------------------------------------

(deftest test-augment-environment-bindings ()
  (check
   (null (set-difference (augment-environment-bindings '((x 2) (y 5)) '((x . 0))) '((x . 2) (y . 5) (x . 0)) :test #'equal))
   (null (set-difference (augment-environment-bindings '(x (y) (z 2)) '((x . 0))) '((x . nil) (y . nil) (z . 2) (x . 0)) :test #'equal))
   (null (set-difference (augment-environment-bindings '((x 2) (y 5) (t 8) (pi 4)) '((x . 0))) '((x . 2) (y . 5) (x . 0)) :test #'equal))))

;; (augment-environment-bindings '(x (y) (z (cons 2 y))) '())
;;   No binding for Y in environment.
;; * (augment-environment-bindings* '(x (y) (z (cons 2 y))) '())
;; ((Z 2) (Y) (X))

(deftest test-augment-environment-bindings* ()
  (check
   (null (set-difference (augment-environment-bindings* '((x 2) (y 5)) '((x . 0))) '((x . 2) (y . 5) (x . 0)) :test #'equal))
   (null (set-difference (augment-environment-bindings* '((x 2) (y (* x 5))) '((x . 0))) '((x . 2) (y . 10) (x . 0)) :test #'equal))
   (null (set-difference (augment-environment-bindings* '(x (y) (z 2)) '((x . 0))) '((x . nil) (y . nil) (z . 2) (x . 0)) :test #'equal))
   (null (set-difference (augment-environment-bindings* '(x (y) (z (cons 2 y))) '((x . 0))) '((x . nil) (y . nil) (z . (2)) (x . 0)) :test #'equal))
   (null (set-difference (augment-environment-bindings* '((x 2) (y 5) (t 8) (pi 4)) '((x . 0))) '((x . 2) (y . 5) (x . 0)) :test #'equal))))

(deftest test-simple-eval ()
  (check
   (= (simple-eval '((lambda (x) (+ x 1)) 9)) 10)
   (= (simple-eval '((lambda (x) (+ x y)) 9) '((y . 3))) 12)
   (= (simple-eval '((lambda (x) (f x 1)) 9) '((f . (lambda (x y) (+ x y)))) ) 10)
   (equal (simple-eval 9) 9)
   (equal (simple-eval "pung") "pung")
   (equal (simple-eval #\a) #\a)
   (equal (simple-eval t) t)
   (equal (simple-eval nil) nil)
   (equal (simple-eval pi) pi)
   (equal (simple-eval :t) :t)
   (equal (simple-eval '(quote pung)) 'pung)
   (simple-eval '(and (< 3 4) (= (+ x 2) 5)) '((x . 3)))
   (not (simple-eval '(and (< 4 3) (= (+ x 2) 5)) '((x . 3))))
   (simple-eval '(or (< 4 3) (= (+ x 2) 5)) '((x . 3)))
   (not (simple-eval '(or (< 4 3) (= (+ x 2) 5)) '((x . 8))))
   (equal (simple-eval '(car '(a b c))) 'a)
   (equal (simple-eval '(cdr '(a b c))) '(b c))
   (equal (simple-eval '(cons (car '(a b c)) (cdr '(a b c)))) '(a b c))
   (equal (simple-eval '(list 1 2 3)) '(1 2 3))
   (equal (simple-eval '(list)) '())
   (equal (simple-eval '(cons (cons 'a 'b) (cons 'c 'd))) '((a . b) . (c . d)))
   (equal (simple-eval '(cons (list 'a 'b) (list 'c 'd))) '((a b) c d))
   (equal (simple-eval '(list (cons 'a 'b) (cons 'c 'd))) '((a . b) (c . d)))
   (not (simple-eval '(zerop x) '((x . 5) (zerop . (lambda (x) (eq x 0)))) ))
   (simple-eval '(zerop x) '((x . 0) (zerop . (lambda (x) (eq x 0)))) )
   (simple-eval '(atom 99))
   (simple-eval '(atom #(1 2 3)))
   (simple-eval '(atom "Is this not pung?"))
   (simple-eval '(eq 'pung 'pung))
   (not (simple-eval '(eq '(pung) '(pung))))
   (simple-eval '(equal '(pung) '(pung)))
   (simple-eval '(equalp "Is this not pung?" "IS THIS NOT PUNG?"))
   (equal (simple-eval '(cond ((zerop x) (print 'pung) (print (eq 5 3)) (car '(a b c))) (t 'foo)) '((x . 0) (zerop . (lambda (x) (eq x 0)))) ) 'a)
   (equal (simple-eval '(cond ((zerop x) (print 'pung) (print (eq 5 3)) (car '(a b c))) (t 'foo)) '((x . 1) (zerop . (lambda (x) (eq x 0)))) ) 'foo)
   (equal (simple-eval '(cond ((zerop x) (print 'pung) (print (eq 5 3)) (car '(a b c)))) '((x . 1) (zerop . (lambda (x) (eq x 0)))) ) nil)
   (equal (simple-eval '(cond ((< 3 3)) (t :foo))) :FOO)
   (equal (simple-eval '(cond ((< 2 3)) (t :foo))) T)
   (equal (simple-eval '(cond ((< 2 3) :bar) (t :foo))) :BAR)
   (equal (simple-eval '(cond ((< 3 3) :bar))) NIL)



   (equal (simple-eval '(if (zerop x) (cons x '()) z) '((x . 0) (z . 9) (zerop . (lambda (x) (eq x 0)))) ) '(0))
   (equal (simple-eval '(if (zerop x) (cons x '()) z) '((x . 1) (z . 9) (zerop . (lambda (x) (eq x 0)))) ) 9)
   (simple-eval '(f (car '(0 1 2))) '((f . (lambda (x) (zerop x))) (zerop . (lambda (y) (eq y 0)))) )
   (not (simple-eval '(f (car '(3 0 1 2))) '((f . (lambda (x) (zerop x))) (zerop . (lambda (y) (eq y 0)))) ))
   (= (simple-eval '(+)) addition-identity)
   (= (simple-eval '(+ . #1=(2))) (cl:+ . #1#))
   (= (simple-eval '(+ . #2=(2 3))) (cl:+ . #2#))
   (= (simple-eval '(+ . #3=(2 3 4))) (cl:+ . #3#))
   (= (simple-eval '(+ . #4=(5 2 3 4))) (cl:+ . #4#))
   (= (simple-eval '(*)) multiplication-identity)
   (= (simple-eval '(* . #5=(2))) (cl:* . #5#))
   (= (simple-eval '(* . #6=(2 3))) (cl:* . #6#))
   (= (simple-eval '(* . #7=(2 3 4))) (cl:* . #7#))
   (= (simple-eval '(* . #8=(5 2 3 4))) (cl:* . #8#))
   (= (simple-eval '(+ (* a x x) (* b x) c) '((a . 3) (b . 4) (c . 9) (x . 10))) 349) ; Base 10 #10r349
   (= (simple-eval '(+ (* a x x) (* b x) c) '((a . 3) (b . 4) (c . 9) (x . 16))) 841) ; Hex #x349
   (= (simple-eval '(/ (- (* x x) 9) (+ x 3)) '((x . 6))) 3) ; (x^2 - 9)/(x + 3) => (x - 3)
   (= (simple-eval '(/ (- (* x x) 16) (- x 4)) '((x . 8))) 12)
   (simple-eval '(= 8))
   (simple-eval '(= 3 3.0))
   (not (simple-eval '(= 3 3.1)))
   (simple-eval '(= (+ 1 4) (/ 10 2) (- 12 7) (* 5/9 9)))
   (simple-eval '(/= 8))
   (simple-eval '(/= 3 3.1))
   (not (simple-eval '(/= 3 3.0)))
   (simple-eval '(/= 2 3 4 5))
   (not (simple-eval '(/= 2 3 4 3)))
   (simple-eval '(< 2))
   (simple-eval '(< 2 8))
   (not (simple-eval '(< 2 2)))
   (not (simple-eval '(< 8 2)))
   (simple-eval '(< 2 8 9))
   (not (simple-eval '(< 2 8 8)))
   (simple-eval '(<= 2))
   (simple-eval '(<= 2 8))
   (simple-eval '(<= 2 2))
   (not (simple-eval '(<= 8 2)))
   (simple-eval '(<= 2 8 9))
   (simple-eval '(<= 2 8 8))
   (not (simple-eval '(<= 2 8 3)))
   (simple-eval '(> 2))
   (simple-eval '(> 8 2))
   (not (simple-eval '(> 2 2)))
   (not (simple-eval '(> 2 8)))
   (simple-eval '(> 9 8 2))
   (not (simple-eval '(> 8 2 2)))
   (simple-eval '(>= 2))
   (simple-eval '(>= 8 2))
   (simple-eval '(>= 2 2))
   (not (simple-eval '(>= 2 8)))
   (simple-eval '(>= 9 8 2))
   (simple-eval '(>= 8 2 2))
   (not (simple-eval '(>= 8 2 7)))
   (eq (simple-eval '(let ((x 8) (y 5)) (if (> x y) :foo :bar))) :FOO)
   (eq (simple-eval '(let ((x 8) (y 8)) (if (> x y) :foo :bar))) :BAR)
   (let ((l (loop for i from 1 to 10 collect i)))
     (notany #'null (loop for i from 1 to 10 ; FIRST ... TENTH
                          for f = (intern (string-upcase (format nil "~:R" i)))
                          collect (eq (simple-eval (list f (list 'quote l)))
                                      (funcall f l)))) )))



;; * (simple-eval '(append '(a b c) '(d e)))
;; (A B C D E)
;; * (simple-eval '(append '(a b c) 'd))
;; (A B C . D)

;; * (simple-eval (read))
;; (subst 'foo 'pung '(is this not pung))
;; (IS THIS NOT FOO)
;; * (subst 'foo 'pung '((pung) (bar (pung)) pung pung ((((pung))))))
;; ((FOO) (BAR (FOO)) FOO FOO ((((FOO)))))
;; * (simple-eval (read))
;; (subst 'foo 'pung '(is this not pung))
;; (IS THIS NOT FOO)
;; * (simple-eval (read))
;; (subst 'foo 'pung '((pung) (bar (pung)) pung pung ((((pung))))))
;; ((FOO) (BAR (FOO)) FOO FOO ((((FOO)))))
;; * (trace simple-eval)
;; (SIMPLE-EVAL)
;; * (simple-eval (read))
;; (subst 'foo 'pung '((pung) (bar (pung)) pung pung ((((pung))))))


;; * (simple-eval (read))
;; (member 'a '(4 9 a c d))
;; (A C D)
;; * (member 'a '(4 9 (a c) d))
;; NIL
;; * (member 'd '(4 9 a c d))
;; (D)
;; * (simple-eval (read))
;; (member 'a '(4 9 (a c) d))
;; NIL
;; * (simple-eval (read))
;; (member 'd '(4 9 a c d))
;; (D)
;; *

;; * (simple-eval (read))
;; (assoc 'f '((a . 1) (b . 2) (c . 3) (d . e)))
;; NIL
;; * (assoc 'f '((a . 1) (b . 2) (c . 3) (d . e)))
;; NIL
;; * (simple-eval (read))
;; (assoc 'c '((a . 1) (b . 2) (c . 3) (d . e)))
;; (C . 3)
;; * (assoc 'c '((a . 1) (b . 2) (c . 3) (d . e)))
;; (C . 3)

;; * (simple-eval (read))
;; (reverse '(a b c d))
;; (D C B A)
;; * (reverse '(a b c d))
;; (D C B A)
;; * (simple-eval (read))
;; (reverse '())
;; NIL
;; * (reverse '())
;; NIL
;; * (simple-eval (read))
;; (reverse '(a (b c) d e))
;; (E D (B C) A)
;; * (reverse '(a (b c) d e))
;; (E D (B C) A)


;; (simple-eval (read))
;; (funcall #'list 1 2 3)
;; (1 2 3)
;; * (simple-eval (read))
;; (reduce #'+ '(1 2 3))
;; 6
;; * (simple-eval (read))
;; (mapcar #'(lambda (x) (* x 5)) '(2 4 6))
;; (10 20 30)
;; * 
