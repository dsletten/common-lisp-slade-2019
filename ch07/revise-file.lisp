;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               revise-file.lisp
;;;;
;;;;   Started:            Mon Aug 12 01:53:36 2019
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

(defpackage :revise-file (:use :common-lisp :test) (:shadow :replace))

(in-package :revise-file)

(defun revise-file (in out pattern replacement)
  (labels ((revise (input output)
             (let ((line (read-line input nil nil)))
               (unless (null line)
                 (let ((revised (string-replace line pattern replacement)))
                   (cond ((string= line revised) (write-line line output))
                         (t (format t "old: ~S~%" line)
                            (format t "new: ~S~2%" revised)
                            (write-line revised output))))
                 (revise input output)))) )
    (with-open-file (input in :if-does-not-exist :error)
      (with-open-file (output out :direction :output :if-exists :supersede)
        (revise input output)))) )

;;;
;;;    Global string replace.
;;;    
(defun string-replace (s pattern replacement)
  "Replace every occurrence of PATTERN in S with REPLACEMENT."
  (let ((length (length pattern)))
    (labels ((replace (stream index)
               (let ((match (search pattern s :start2 index)))
                 (cond (match (write-string (subseq s index match) stream)
                              (write-string replacement stream)
                              (replace stream (+ match length)))
                       (t (write-string (subseq s index) stream)))) ))
      (with-output-to-string (stream)
        (replace stream 0)))) )

(deftest test-string-replace ()
  (check
   (string= (string-replace "Is this not pung?" "pung" "FOO") "Is this not FOO?")
   (string= (string-replace "ab cdab bd abab cab" "ab" "xxx") "xxx cdxxx bd xxxxxx cxxx")
   (string= (string-replace #1="Is this not pung?" "PUNG" "FOO") #1#)))
   