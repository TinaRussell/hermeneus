;;; hermeneus-test.el --- Tests for Hermeneus -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

(require 'ert)
(require 'hermeneus)

;; [[id:TKR:ac0367f5-9016-4139-acef-3183123bb0d6][Testing:1]]
(ert-deftest hermeneus-conv-test ()
  "Test Beta to Unicode conversion"
  (should (equal (hermeneus-beta-to-unicode "*)odusseu/s") "Ὀδυσσεύς"))
  (should (let ((hermeneus-beta-input-type 'greek-kbd))
            (hermeneus-conv--set-beta-input-type)
            (equal (hermeneus-beta-to-unicode "*)odyssey/w" t) "Ὀδυσσεύς")))
  (should (let ((hermeneus-beta-input-type '("o" "d" "y" "b" "z" "`"  "e" "s" "w" "i" "ĳ" "k" "l" "m"
                                       "n" "j" "a" "p" "r" "hu" "t" "g" "f" "x" "c" "v"
                                       "O" "D" "Y" "B" "Z" "~"  "E" "S" "W" "I" "Ĳ" "K" "L" "M"
                                       "N" "J" "A" "P" "R" "HU" "T" "G" "F" "X" "C" "V")))
            (hermeneus-conv--set-beta-input-type)
            (equal (hermeneus-beta-to-unicode "*)abghhzg/u" t) "Ὀδυσσεύς"))))
;; Testing:1 ends here

;; [[id:TKR:8999aa73-3aa5-46fa-be90-be901b945d1e][Testing:1]]
(ert-deftest hermeneus-match-test ()
  "Test the results of the regexp matcher."
  ;; This is the sexp used to create ‘wordlist’:
  ;; (cl-loop for string in (hash-table-keys (oref hermeneus-lsj entries)) if (string-match-p (hermeneus--re-builder "αφρο") string) collect string)
  (should (let ((wordlist '("ἀκαταφρόνητος" "ἀναφροδισία"
                            "ἀναφρόδιτος" "ἀναφρονέω" "ἀναφροντίζω"
                            "ἄναφρος" "ἀνεπαφροδισία" "ἀνεπαφρόδιτος"
                            "ἀντικαταφρονέω" "ἀξιοκαταφρόνητος"
                            "ἀφρόγαλα" "ἀφρογένεια" "Ἀφροδίσια"
                            "Ἀφροδισιάζω" "Ἀφροδισιακός" "Ἀφροδισιάς"
                            "Ἀφροδισιασμός" "Ἀφροδισιαστής"
                            "Ἀφροδισιαστικός" "Ἀφροδίσιος"
                            "Ἀφροδισιών" "Ἀφροδιταρίδιον"
                            "Ἀφροδιτάριον" "Ἀφροδίτη" "ἀφρόκομος"
                            "ἀφρόλιτρον" "ἀφρονεύομαι" "ἀφρόνευσις"
                            "ἀφρονέω" "ἀφρόνη" "ἀφρόνησις" "ἀφρονίζω"
                            "ἀφρονικός" "ἀφρόνιτρον" "ἄφροντις"
                            "ἀφροντιστέω" "ἀφροντιστητέον"
                            "ἀφροντιστί" "ἀφροντιστία" "ἀφρόντιστος"
                            "ἀφρόομαι" "ἀφρός" "ἀφροσέληνος"
                            "ἀφροσιβόμβαξ" "ἀφρόσκορδον" "ἀφροσύνη"
                            "ἀφροτόκος" "ἀφρουρέω" "ἀφρούρητος"
                            "ἄφρουρος" "ἀφροφυής" "δειλοκαταφρονητής"
                            "διαφρονέω" "διαφροντίζω" "διαφρος"
                            "διαφρουρέω" "δυσκαταφρόνητος"
                            "ἐλαφρόγειος" "ἐλαφρόνοος" "ἐλαφρός"
                            "ἐλαφρότης" "ἐλαφροτοκία" "ἐλαφρόω"
                            "ἐναφροδισιάζω" "ἐξαφρόομαι" "ἐπαφροδισία"
                            "ἐπαφρόδιτος" "ἔπαφρος" "Ἑρμαφρόδιτος"
                            "εὐαφρόδιτος" "εὐκαταφρόνητος"
                            "εὐκαταφρόντιστος" "καταφρονέω"
                            "καταφρόνημα" "καταφρόνησις"
                            "καταφρονητέον" "καταφρονητής"
                            "καταφρονητικός" "καταφρόνητος"
                            "καταφροντίζω" "Κουραφροδίτη" "ναφρόν"
                            "πανεπαφροδισία" "παραφρονέω" "παραφρονία"
                            "παραφρόνιμος" "παραφροσύνη" "παραφρουρέω"
                            "προσκαταφρονέω" "ταφροβολέω" "ταφροειδής"
                            "ταφροποιέω" "τάφρος" "ὕπαφρος"
                            "ὑπελαφρός" "ὑπερέλαφρος"
                            "ὑποκαταφρονέω"))
                (regexp (hermeneus--re-builder "Αφροδ"))
                (expected-result '("Ἀφροδίσια" "Ἀφροδισιάζω"
                                   "Ἀφροδισιακός" "Ἀφροδισιάς"
                                   "Ἀφροδισιασμός" "Ἀφροδισιαστής"
                                   "Ἀφροδισιαστικός" "Ἀφροδίσιος"
                                   "Ἀφροδισιών" "Ἀφροδιταρίδιον"
                                   "Ἀφροδιτάριον" "Ἀφροδίτη"
                                   "ἀναφροδισία" "ἀναφρόδιτος"
                                   "ἀνεπαφροδισία" "ἀνεπαφρόδιτος"
                                   "ἐναφροδισιάζω" "ἐπαφροδισία"
                                   "ἐπαφρόδιτος" "Ἑρμαφρόδιτος"
                                   "εὐαφρόδιτος" "Κουραφροδίτη"
                                   "πανεπαφροδισία"))
                (case-fold-search t))
            (equal (hermeneus--re-matcher regexp wordlist)
                   expected-result))))
;; Testing:1 ends here

;; [[id:TKR:98034f7e-a248-4c5a-a6d5-69b803a991c8][Testing:1]]
(ert-deftest hermeneus-storage-test ()
  "Test the object and storage facilities of Hermeneus."
  (let* ((file (make-temp-file "hermeneus-storage-test" nil
                               (when (executable-find "gzip") ".gz")))
         (key "Ἑρμῆς"))
    (unwind-protect
        (progn
          ;; Make a word object
          (let ((word (hermeneus-word :key key :id 42622 :loc '(4 31300602 31327799)))
                ;; Make a lexicon object
                (lexicon (hermeneus-lexicon :file file)))
            ;; Add the word to the lexicon
            (puthash key word (oref lexicon entries))
            ;; Save the lexicon object
            (eieio-persistent-save lexicon))
          ;; Open the lexicon object from the file
          (let* ((lexicon (eieio-persistent-read file 'hermeneus-lexicon))
                 ;; Access the word object from it
                 (word (gethash key (oref lexicon entries))))
            ;; Finally, the tests
            (cl-check-type word hermeneus-word)
            (should (= (oref word id) 42622))
            (should (equal (oref word loc) '(4 31300602 31327799)))))
      ;; Clean up
      (delete-file file))))
;; Testing:1 ends here

;; [[id:TKR:6a32401f-f9e7-4e52-9c47-65cc722cb2ba][Testing:1]]
(ert-deftest hermeneus-xml-test ()
  "Test the XML functionality in Hermeneus."
  (should (hermeneus--url-p "https://sega.com/"))
  (should-not (hermeneus--url-p user-emacs-directory))
  ;; Shadow some variables
  (let* ((temp-storage-dir (make-temp-file "hermeneus-" t))
         (hermeneus-storage-dir temp-storage-dir)
         hermeneus-storage-path
         ;; Here we set ‘hermeneus-lsj-dir’, temporarily, to its
         ;; default value, which is the location of the LSJ in the
         ;; PerseusDL “lexica” repository. I’m wondering if we should
         ;; set it the same location in a fork of the repository, to
         ;; make sure it stays consistent between tests. I don’t know.
         (hermeneus-lsj-dir hermeneus--git-lsj-dir)
         hermeneus-lsj-files)
    (hermeneus--set-storage-dir)
    (hermeneus--set-lsj-dir)
    (unwind-protect
        (let* ((hermeneus-lsj (hermeneus-lexicon))
               (entries (hermeneus-get-entries hermeneus-lsj))
               (word (gethash "Ἀφροδίτη" entries))
               (dom (hermeneus--get-dom-from-word word)))
          ;; Finally, the actual tests
          (cl-check-type word hermeneus-word)
          (should (dom-ensure-node dom))
          (should (eq (dom-tag dom) 'entryFree))
          (should (string= (thread-first dom
                                         (dom-child-by-tag 'sense)
                                         (dom-child-by-tag 'tr)
                                         (dom-text))
                           "Aphrodite,")))
      ;; Clean up
      (delete-directory temp-storage-dir t))))
;; Testing:1 ends here

;; [[id:TKR:ba63b06f-cf08-4ff1-9b1f-eed5387a6a47][Testing:1]]

;; Testing:1 ends here

;; [[id:TKR:2f830a16-8963-4182-a998-c45bfa2401c1][Testing:1]]
(ert-deftest hermeneus-cts-test ()
  "Test the Canonical Test Services functionality."
  (should (equal (hermeneus-urn-to-base "urn:cts:greekLit:tlg0020.tlg001.perseus-grc1:195")
                 "urn:cts:greekLit:tlg0020.tlg001.perseus-grc1"))
  (should (equal (hermeneus-urn-to-work "urn:cts:greekLit:tlg0020.tlg001.perseus-grc1:195")
                 "urn:cts:greekLit:tlg0020.tlg001"))
  (should (equal (hermeneus-urn-to-url "urn:cts:greekLit:tlg0020.tlg001.perseus-grc1:195")
                 "http://data.perseus.org/catalog/urn:cts:greekLit:tlg0020.tlg001.perseus-grc1")))
;; Testing:1 ends here

;; [[id:TKR:0822da09-649c-47fe-96ec-e53bb1ff6043][Testing:1]]

;; Testing:1 ends here

;; [[id:TKR:616b86f6-97e1-4eef-b5f6-4d34c45a492b][Testing:1]]

;; Testing:1 ends here

;;; hermeneus-test.el ends here
