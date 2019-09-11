;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch08.lisp
;;;;
;;;;   Started:            Thu Aug 15 00:33:41 2019
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

(defpackage :ch08 (:use :common-lisp :test) (:shadow :reverse))

(in-package :ch08)


;;;
;;;    8.7.2 Quine (See ~/lisp/programs/quine.lisp)
;;;
((lambda (l) (list l (list 'quote l)))
 '(lambda (l) (list l (list 'quote l))))

; Test it!! (equal + *)

;;;
;;;    8.7.3
;;;    Work backward from end of string to create circular list of its chars.
;;;
(defun make-circular-key (key)
  (assert (string/= key ""))
  (labels ((construct (i result final)
             (cond ((minusp i) (setf (rest final) result)
                    result)
                   (t (construct (1- i) (cons (char key i) result) final)))) )
    (let* ((i (1- (length key)))
           (final (list (char key i))))
      (construct (1- i) final final))))

(defun make-circular-key (key)
  (assert (string/= key ""))
  (labels ((construct (i result final)
             (push (char key i) result)
             (if (zerop i)
                 (setf (rest final) result)
                 (construct (1- i) result final))))
    (let* ((i (1- (length key)))
           (final (list (char key i))))
      (if (zerop i) ; D'oh! Special corner case...
          (progn (setf (rest final) final)
                 final)
          (construct (1- i) final final)))) )

(defun make-circular-key (key)
  (assert (string/= key ""))
  (let* ((last (1- (length key)))
         (final (list (char key last)))
         (result final))
    (loop for i from (1- last) downto 0
          do (push (char key i) result)
          finally (setf (rest final) result))
    result))

(defun check-circular-key (s)
  (let ((length (length s))
        (circle (make-circular-key s)))
    (string= s
             (coerce (loop repeat length
                           collect (pop circle))
                     'string))))

(deftest test-make-circular-key ()
  (check
   (check-circular-key "Is this not pung?")
   (check-circular-key "x")))

(defun make-key-stream-reader (key)
  "Return a closure over the string KEY that implements an endless stream of chars cycling from that string."
  (let ((stream (make-string-input-stream key)))
  #'(lambda ()
      (unless (listen stream)
        (setf stream (make-string-input-stream key)))
      (read-char stream))))

(defun string-encode (plain-text key divisor)
  "Encode PLAIN-TEXT with the characters of the text KEY modified by DIVISOR."
  (let ((reader (make-key-stream-reader key)))
    (with-output-to-string (encoded)
      (dotimes (i (length plain-text))
        (write-char (encode-char (char plain-text i) (funcall reader) divisor) encoded)))) )

(defun string-encode (plain-text key divisor)
  "Encode PLAIN-TEXT with the characters of the text KEY modified by DIVISOR."
  (let ((reader (make-key-stream-reader key)))
    (with-output-to-string (encoded)
      (loop for ch across plain-text
            do (write-char (encode-char ch (funcall reader) divisor) encoded)))) )

(defun string-encode (plain-text key divisor)
  (map 'string #'(lambda (ch1 ch2) (encode-char ch1 ch2 divisor)) plain-text (make-circular-key key)))

(defun encode-char (ch1 ch2 m)
  (code-char (+ (char-code ch1) (rem (char-code ch2) m))))

(defun string-decode (encoded-text key divisor)
  "Decode ENCODED-TEXT using the characters of KEY modified by DIVISOR."
  (let ((reader (make-key-stream-reader key)))
    (with-output-to-string (plain)
      (dotimes (i (length encoded-text))
        (write-char (decode-char (char encoded-text i) (funcall reader) divisor) plain)))) )

(defun string-decode (encoded-text key divisor)
  (map 'string #'(lambda (ch1 ch2) (decode-char ch1 ch2 divisor)) encoded-text (make-circular-key key)))

(defun decode-char (ch1 ch2 m)
  (code-char (- (char-code ch1) (rem (char-code ch2) m))))

(deftest test-string-encode ()
  (check
   (string= (string-encode "Mary had a little lambda" "He who hesitates is last." 7) "Odvy&nej#d poxwoi oephge")
   (string= (string-decode (string-encode #1="Mary had a little lambda" #2="He who hesitates is last." #3=7) #2# #3#) #1#)))

(deftest test-string-decode ()
  (check
   (string= (string-decode "Odvy&nej#d poxwoi oephge" "He who hesitates is last." 7) "Mary had a little lambda")
   (string= (string-encode (string-decode #1="Odvy&nej#d poxwoi oephge" #2="He who hesitates is last." #3=7) #2# #3#) #1#)))

;;;
;;;    8.7.4
;;;
(defvar *suits* '(clubs diamonds hearts spades))
(defvar *ranks* (append (loop for i from 2 to 10 collect (intern (string-upcase (format nil "~R" i)))) '(jack queen king ace)))

(defun make-deck (ranks suits)
  (mapcan #'(lambda (suit)
              (mapcar #'(lambda (rank)
                          (cons rank suit))
                      ranks))
          suits))

;;;
;;;    8.7.5
;;;
(defun reverse (l)
  (labels ((reverse-aux (l result)
             (if (endp l)
                 result
                 (reverse-aux (rest l) (cons (first l) result)))) )
    (reverse-aux l '())))

(deftest test-reverse ()
  (check
   (equal (reverse '()) '())
   (equal (reverse '(a)) '(a))
   (equal (reverse '(a b)) '(b a))
   (equal (reverse '(a b c)) '(c b a))
   (equal (reverse '(a b (1 2) c)) '(c (1 2) b a))))

;;;
;;;    8.7.6
;;;
(defun power (set)
  (cond ((null set) '(()))
        (t (let ((result (power (rest set))))
             (append (mapcar #'(lambda (subset) (cons (first set) subset)) result) result)))) )


(defun power-set (set)
  (cond ((null set) (list '()))
        (t (layer-elt (first set) (power-set (rest set)))) ))

(defun layer-elt (elt set)
  (labels ((layer (set1)
             (if (null set1)
                 set
                 (cons (cons elt (first set1))
                       (layer (rest set1)))) ))
    (layer set)))

(defun set-equal (s1 s2)
  (and (subsetp s1 s2 :test #'equal)
       (subsetp s2 s1 :test #'equal)))

(deftest test-power-set ()
  (check
   (set-equal (power '()) '(NIL))
   (set-equal (power '(a)) '((A) NIL))
   (set-equal (power '(a b)) '((A B) (A) (B) NIL))
   (set-equal (power '(a b c)) '((A B C) (A B) (A C) (A) (B C) (B) (C) NIL))
   (set-equal (power '(a b c d)) '((A B C D) (A B C) (A B D) (A B) (A C D) (A C) (A D) (A) (B C D) (B C) (B D) (B) (C D) (C) (D) NIL))))
