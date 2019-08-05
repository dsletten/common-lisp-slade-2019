;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp isn't a language, it's a building material.
;;;;   -- Alan Kay
;;;;
;;;;   Name:               spell-correct.lisp
;;;;
;;;;   Started:            Sat Aug  3 01:12:46 2019
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
(load "/Users/dsletten/lisp/packages/io.lisp")

(defpackage :spell-correct (:use :common-lisp :test :io))

(in-package :spell-correct)

(defun tag-word (word)
  (setf (get word 'isa-word) t))

;; (defvar *dictionary* '(commuter computer computation computing compute computers))
;; (dolist (word *dictionary*)
;;   (tag-word word))

(defvar *dictionary* (make-hash-table :test #'equalp))
(when (zerop (hash-table-count *dictionary*))
  (dolist (word (read-file "/Users/dsletten/lisp/books/Slade/ch06/wordlists/words.big"))
    (if (gethash word *dictionary*)
        (format t "Already encountered: ~A~%" word)
        (setf (gethash word *dictionary*) t))))

(defun spell-match (word)
  (if (symbolp word)
      (find-match (symbol-name word))
      (find-match word)))

(defun find-match (word)
  (let ((length (length word)))
    (cond ((zerop length) nil)
          ((check-word word))
          ((check-deletion word length))
          ((check-transposition word (1- length)))
          ((check-double word (1- length)))
          ((check-insertion word length))
          (t nil))))

;; (defun check-word (word)
;;   (let ((symbol (find-symbol (string-upcase word))))
;;     (cond ((null symbol) nil)
;;           ((get symbol 'isa-word) word)
;;           (t nil))))

(defun check-word (word)
  (if (gethash word *dictionary*)
      word
      nil))

(defun check-deletion (s i)
  (if (zerop i)
      nil
      (let ((new-word (concatenate 'string (subseq s 0 (1- i)) (subseq s i))))
        (or (check-word new-word)
            (check-deletion s (1- i)))) ))

(defun check-transposition (s i)
  (if (zerop i)
      nil
      (or (check-word (string-swap s i (1- i)))
          (check-transposition s (1- i)))) )

(defun string-swap (s i j)
  (let ((result (copy-seq s)))
    (rotatef (char result i) (char result j))
    result))

(deftest test-string-swap ()
  (check
   (string= (string-swap "hello there" 3 5) "hel olthere")
   (string= (string-swap "hello there" 5 9) "hellorthe e")))

(defun check-double (s i)
  (if (minusp i)
      nil
      (or (check-word (string-insert s (char s i) i))
          (check-double s (1- i)))) )

(defun string-insert (s ch i) ; !!
  (concatenate 'string (subseq s 0 i) (string ch) (subseq s i)))

(deftest test-string-insert ()
  (check
   (string= (string-insert "hello" #\X 0) "Xhello")
   (string= (string-insert "hello" #\X 1) "hXello")
   (string= (string-insert "hello" #\X 4) "hellXo")
   (string= (string-insert "hello" #\X 5) "helloX")))

(defun check-insertion (s i)
  (if (minusp i)
      nil
      (or (insert s i #\A)
          (check-insertion s (1- i)))) )

(defun insert (s i ch) ; !!
  (if (char-greaterp ch #\Z)
      nil
      (or (check-word (string-insert s ch i))
          (insert s i (next-char ch)))) )

(defun next-char (ch)
  (code-char (1+ (char-code ch))))
