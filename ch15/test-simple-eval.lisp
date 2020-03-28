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

(deftest test-augment-environment ()
  (check
   (equal (augment-environment '(x y) '(2 5) '((x . 0))) '((x . 2) (y . 5) (x . 0)))
   (equal (augment-environment '(x y) '(2) '((x . 0))) '((x . 2) (y . nil) (x . 0)))) )

(deftest test-simple-eval ()
  (check
   (equal (simple-eval 9) 9)
   (equal (simple-eval "pung") "pung")
   (equal (simple-eval #\a) #\a)
   (equal (simple-eval t) t)
   (equal (simple-eval nil) nil)
   (equal (simple-eval pi) pi)
   (equal (simple-eval :t) :t)
   (equal (simple-eval '(quote pung)) 'pung)
   (equal (simple-eval '(car '(a b c))) 'a)
   (equal (simple-eval '(cdr '(a b c))) '(b c))
   (equal (simple-eval '(cons (car '(a b c)) (cdr '(a b c)))) '(a b c))
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
   (not (simple-eval '(>= 8 2 7)))) )






