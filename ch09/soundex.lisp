;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               soundex.lisp
;;;;
;;;;   Started:            Sun Sep 15 00:26:40 2019
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
;;;;   Notes: See Notes sections from 2004/2011 versions.
;;;;   https://en.wikipedia.org/wiki/Metaphone
;;;;   http://aspell.net/metaphone
;;;;   http://aspell.net/
;;;;   http://suggest.aspell.net/
;;;;
;;;;   https://west-penwith.org.uk/misc/soundex.htm
;;;;
;;;;
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :soundex (:use :common-lisp :test))

(in-package :soundex)

(defconstant soundex-length 4)
(defconstant wh-code 7)
(defconstant vowel-code 0)

;;;
;;;    This is my implementation of his description of the algorithm.
;;;    His actual implementation is junk!
;;;    
(defun slade-soundex (name)
  (assert (and (stringp name) (string/= name "")))
  (let ((initial (char-upcase (char name 0)))
        (prepped-name (remove-pairs-string (string-downcase name))))
    (format-soundex (coerce (cons initial (mapcar #'digit-char (loop for ch across (subseq prepped-name 1)
                                                                     when (slade-soundex-map ch)
                                                                     collect it)))
                            'string))))

(defun soundex (name)
  (nara-soundex (remove #\space name)))

;;;
;;;    Remove adjacent code pairs unless separated by vowel.
;;;    (But remove vowels before final processing.)
;;;    
(defun nara-soundex (name)
  (assert (and (stringp name) (string/= name "")))
  (let ((initial (char-upcase (char name 0)))
        (prepped-name (remove-numeric-pairs (remove wh-code (name->nara name)))))
    (format-soundex (coerce (cons initial
                                  (mapcar #'digit-char
                                          (remove vowel-code (rest prepped-name))))
                            'string))))

(defun name->nara (name)          
  "Convert the name to a list of letter codes."
  (loop for ch across (string-downcase name) collect (nara-soundex-map ch)))

(defun remove-pairs-string (s)
  (with-input-from-string (in s)
    (with-output-to-string (out)
      (do* ((ch1 (read-char in nil nil) ch2)
            (ch2 (read-char in nil nil) (read-char in nil nil)))
           ((null ch1))
        (when (or (null ch2) (char/= ch1 ch2))
          (write-char ch1 out)))) ))

(defun remove-numeric-pairs (l)
  (remove-pairs l #'=))

(defun remove-pairs (l f)
  (loop for cons on l
        when (or (endp (rest cons))
                 (not (funcall f (first cons) (second cons))))
        collect (first cons)))

(defun slade-soundex-map (ch)
  (case ch
    ((#\b #\f #\p #\v) 1)
    ((#\c #\g #\j #\k #\q #\x) 2)
    ((#\d #\t) 3)
    (#\l 4)
    ((#\m #\n) 5)
    (#\r 6)
    ((#\s #\z) 7)))

(defun nara-soundex-map (ch)
  (case ch
    ((#\b #\f #\p #\v) 1)
    ((#\c #\g #\j #\k #\q #\s #\x #\z) 2)
    ((#\d #\t) 3)
    (#\l 4)
    ((#\m #\n) 5)
    (#\r 6)
    ((#\w #\h) wh-code) ; Used to flag for removal before collapsing pairs.
    (otherwise vowel-code)))

(defun format-soundex (code)
  (let ((result (make-string soundex-length :initial-element #\0)))
    (setf (subseq result 0 soundex-length) code)
    result))

(let ((dictionary (make-hash-table :test #'equalp))
      (reverse-dictionary (make-hash-table :test #'equalp)))
  (defun tag-word (word)
    (let ((code (soundex word)))
      (setf (gethash word dictionary) code)
      (setf (gethash code reverse-dictionary) word)))
  (defun isa-word (w)
    (let ((code (gethash w dictionary)))
      (if code
          t
          (gethash (soundex w) reverse-dictionary)))) )

(defvar *states* '("Alabama" "Alaska" "Arizona" "Arkansas" "California" "Colorado" "Connecticut" "Delaware" "Florida" "Georgia" "Hawaii" "Idaho" "Illinois" "Indiana" "Iowa" "Kansas" "Kentucky" "Louisiana" "Maine" "Maryland" "Massachusetts" "Michigan" "Minnesota" "Mississippi" "Missouri" "Montana" "Nebraska" "Nevada" "New Hampshire" "New Jersey" "New Mexico" "New York" "North Carolina" "North Dakota" "Ohio" "Oklahoma" "Oregon" "Pennsylvania" "Rhode Island" "South Carolina" "South Dakota" "Tennessee" "Texas" "Utah" "Vermont" "Virginia" "Washington" "West Virginia" "Wisconsin" "Wyoming"))

(dolist (state *states*) (tag-word state))

(deftest test-slade-soundex ()
  (check
   (string= (slade-soundex "harry") "H600")
   (string= (slade-soundex "hairy") "H600")
   (string= (slade-soundex "missouri") "M760")
   (string= (slade-soundex "arizona") "A675")
   (string= (slade-soundex "arkansas") "A625")
   (string= (slade-soundex "williams") "W457")
   (string= (slade-soundex "baragwanath") "B625")
   (string= (slade-soundex "donnell") "D540")
   (string= (slade-soundex "lloyd") "L300")
   (string= (slade-soundex "woolcock") "W422")
   (string= (slade-soundex "ashcraft") "A726")
   (string= (slade-soundex "Tymczak") "T527")
   (string= (slade-soundex "Jackson") "J227")
   (string= (slade-soundex "Pfister") "P173")))

(deftest test-soundex-2019 ()
  (check
   (string= (soundex "harry") "H600")
   (string= (soundex "hairy") "H600")
   (string= (soundex "missouri") "M260")
   (string= (soundex "arizona") "A625")
   (string= (soundex "arkansas") "A625")

   ;;    Knuth pg. 395
   (string= (soundex "Euler") "E460")
   (string= (soundex "Ellery") "E460")
   (string= (soundex "Gauss") "G200")
   (string= (soundex "Ghosh") "G200")
   (string= (soundex "Hilbert") "H416")
   (string= (soundex "Heilbronn") "H416")
   (string= (soundex "Knuth") "K530")
   (string= (soundex "Kant") "K530")
   (string= (soundex "Lloyd") "L300")
   (string= (soundex "Liddy") "L300")
   (string= (soundex "Lukasiewicz") "L222")
   (string= (soundex "Lissajous") "L222")
   (string= (soundex "Wachs") "W200")
   (string= (soundex "Waugh") "W200")

   (string= (soundex "williams") "W452")
   (string= (soundex "baragwanath") "B625")
   (string= (soundex "donnell") "D540")
   (string= (soundex "lloyd") "L300")
   (string= (soundex "woolcock") "W422")

   (string= (soundex "ashcraft") "A261")
   (string= (soundex "ashcroft") "A261")
   (string= (soundex "vandeusen") "V532")
   (string= (soundex "deusen") "D250")
   (string= (soundex "Washington") "W252")
   (string= (soundex "Lee") "L000")
   (string= (soundex "Tymczak") "T522")
   (string= (soundex "Jackson") "J250")
   (string= (soundex "Pfister") "P236")

   (string= (soundex "Cockburn") "C216")
   (string= (soundex "BARHAM") "B650")
   (string= (soundex "BARONE") "B650")
   (string= (soundex "BARRON") "B650")
   (string= (soundex "BERNA") "B650")
   (string= (soundex "BIRNEY") "B650")
   (string= (soundex "BIRNIE") "B650")
   (string= (soundex "BOOROM") "B650")
   (string= (soundex "BOREN") "B650")
   (string= (soundex "BORN") "B650")
   (string= (soundex "BOURN") "B650")
   (string= (soundex "BOURNE") "B650")
   (string= (soundex "BOWRON") "B650")
   (string= (soundex "BRAIN") "B650")
   (string= (soundex "BRAME") "B650")
   (string= (soundex "BRANN") "B650")
   (string= (soundex "BRAUN") "B650")
   (string= (soundex "BREEN") "B650")
   (string= (soundex "BRIEN") "B650")
   (string= (soundex "BRIM") "B650")
   (string= (soundex "BRIMM") "B650")
   (string= (soundex "BRINN") "B650")
   (string= (soundex "BRION") "B650")
   (string= (soundex "BROOM") "B650")
   (string= (soundex "BROOME") "B650")
   (string= (soundex "BROWN") "B650")
   (string= (soundex "BROWNE") "B650")
   (string= (soundex "BRUEN") "B650")
   (string= (soundex "BRUHN") "B650")
   (string= (soundex "BRUIN") "B650")
   (string= (soundex "BRUMM") "B650")
   (string= (soundex "BRUN") "B650")
   (string= (soundex "BRUNO") "B650")
   (string= (soundex "BRYAN") "B650")
   (string= (soundex "BURIAN") "B650")
   (string= (soundex "BURN") "B650")
   (string= (soundex "BURNEY") "B650")
   (string= (soundex "BYRAM") "B650")
   (string= (soundex "BYRNE") "B650")
   (string= (soundex "BYRON") "B650")
   (string= (soundex "BYRUM") "B650")

   (string= (soundex "Allricht") "A462")
   (string= (soundex "Eberhard") "E166")
   (string= (soundex "Engebrethson") "E521")
   (string= (soundex "Heimbach") "H512")
   (string= (soundex "Hanselmann") "H524")
   (string= (soundex "Hildebrand") "H431")
   (string= (soundex "Kavanagh") "K152")
   (string= (soundex "Lind") "L530")
   (string= (soundex "Lukaschowsky") "L222")
   (string= (soundex "McDonnell") "M235")
   (string= (soundex "McGee") "M200")
   (string= (soundex "Opnian") "O155")
   (string= (soundex "Oppenheimer") "O155")
   (string= (soundex "Riedemanas") "R355")
   (string= (soundex "Zita") "Z300")
   (string= (soundex "Zitzmeinn") "Z325")

   (string= (soundex "HOLMES") "H452")
   (string= (soundex "ADOMOMI") "A355")
   (string= (soundex "VONDERLEHR") "V536")
   (string= (soundex "BALL") "B400")
   (string= (soundex "SHAW") "S000")
   (string= (soundex "JACKSON") "J250")
   (string= (soundex "SCANLON") "S545")
   (string= (soundex "SAINTJOHN") "S532")

   (string= (soundex "Sgler") "S460")
   (string= (soundex "Swhgler") "S460")))




