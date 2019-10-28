;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch09.lisp
;;;;
;;;;   Started:            Tue Sep 10 21:31:48 2019
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

(defpackage :ch09 (:use :common-lisp :test))

(in-package :ch09)

;;;
;;;    9.12.2
;;;
(defun average (l)
  (if (null l)
      0
      (let ((sum 0)
            (count 0))
        (loop
           (incf sum (pop l))
           (incf count)
           (when (null l)
             (return (/ sum count)))) )))

(deftest test-average ()
  (check
   (zerop (average '()))
   (= (average '(1)) 1)
   (= (average '(1 2)) 3/2)
   (= (average (loop for i from 1 to 100 collect i)) (/ 5050 100))))

;;;
;;;    9.12.3
;;;
;; (defun remove-pairs (s)
;;   (if (string= s "")
;;       ""
;;       (with-output-to-string (s1)
;;         (let ((ch (char s 0)))
;;           (write-char ch s1)
;;           (do ((i 1 (1+ i)))
;;               ((= i (length s)))
;;             (unless (char= ch (char s i))
;;               (setf ch (char s


;;;
;;;    Weird mix of initial corner cases, aux functions returning values/side effects...
;;;    
(defun remove-pairs (s)
  (labels ((process-string (ch i stream)
             (write-char ch stream)
             (unless (= i (length s))
               (skip-chars ch i stream)))
           (skip-chars (ch i stream)
             (cond ((= i (length s)) nil)
                   ((char= ch (char s i)) (skip-chars ch (1+ i) stream))
                   (t (process-string (char s i) i stream)))) )
    (if (string= s "")
        ""
        (let ((stream (make-string-output-stream)))
          (process-string (char s 0) 0 stream)
          (get-output-stream-string stream)))) )

(defun remove-pairs (s)
  (labels ((process-string (ch in out)
             (unless (null ch)
               (write-char ch out)
               (process-string (skip-chars ch (read-char in nil nil) in) in out)))
           (skip-chars (ch ch1 in)
             (if (or (null ch1) (char/= ch ch1))
                 ch1
                 (skip-chars ch (read-char in nil nil) in))))
    (with-input-from-string (in s)
      (with-output-to-string (out)
        (process-string (read-char in nil nil) in out)))) )

(defun remove-pairs (s) ; Cool 1
  (with-input-from-string (in s)
    (with-output-to-string (out)
      (labels ((process-string (current-char)
                 (unless (null current-char)
                   (write-char current-char out)
                   (process-string (skip-chars current-char (read-char in nil nil)))) )
               (skip-chars (current-char next-char)
                 (cond ((null next-char) nil)
                       ((char= current-char next-char) (skip-chars current-char (read-char in nil nil)))
                       (t next-char))))
        (process-string (read-char in nil nil)))) ))

(defun remove-pairs (s)
  (if (< (length s) 2)
      s
      (with-output-to-string (out)
        (write-char (char s 0) out)
        (do ((length (length s))
             (i 0 (1+ i))
             (j 1 (1+ j)))
            ((= j length))
          (unless (char= (char s i) (char s j))
            (write-char (char s j) out)))) ))

(defun remove-pairs (s) ; Cool 2
  (with-input-from-string (in s)
    (with-output-to-string (out)
      (do* ((ch1 (read-char in nil nil) ch2)
            (ch2 (read-char in nil nil) (read-char in nil nil)))
           ((null ch1))
        (when (or (null ch2) (char/= ch1 ch2))
          (write-char ch1 out)))) ))

;; (defun remove-pairs (s)
;;   (if (< (length s) 2)
;;       s
;;       (map 'list #'(lambda (ch1 ch2)

;;;
;;;    Derived from 2004 Soundex
;; (defun remove-pairs (list)
;;   (let* ((previous (first list))
;;          (result (list previous)))
;;     (dolist (current (rest list) (nreverse result))
;;       (unless (eql current previous)
;;         (push current result))
;;       (setf previous current))))

(defun remove-pairs (s)
  (if (string= s "")
      s
      (with-output-to-string (out)
        (let ((previous (char s 0)))
          (write-char previous out)
          (do ((i 1 (1+ i)))
              ((= i (length s)))
            (let ((current (char s i)))
              (unless (char= current previous)
                (write-char current out))
              (setf previous current)))) )))
                     
(defun remove-pairs (s)
  (with-output-to-string (out)
    (labels ((process-string (i previous)
               (unless (= i (length s))
                 (let ((current (char s i)))
                   (unless (char= current previous)
                     (write-char current out))
                   (process-string (1+ i) current)))) )
      (if (string= s "")
          s
          (let ((ch (char s 0)))
            (write-char ch out)
            (process-string 1 ch)))) ))

(defun remove-pairs (s)
  (coerce (loop for cons on (coerce s 'list)
                when (or (endp (rest cons))
                         (not (eql (first cons) (second cons))))
                collect (first cons)) 'string))

(defun remove-pairs (s)
  (let ((result '()))
    (mapl #'(lambda (cons) (when (or (endp (rest cons)) (not (char= (first cons) (second cons)))) (push (first cons) result))) (coerce s 'list))
    (coerce (nreverse result) 'string)))

(deftest test-remove-pairs ()
  (check
   (string= (remove-pairs "") "")
   (string= (remove-pairs "k") "k")
   (string= (remove-pairs #1="Is this not pung?") #1#)
   (string= (remove-pairs "aaabbb") "ab")
   (string= (remove-pairs "aaabbbcdccccddd") "abcdcd")
   (string= (remove-pairs "bookeeper") "bokeper")))
