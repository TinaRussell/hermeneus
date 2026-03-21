;; [[id:TKR:654eb5b3-da3c-44aa-a53b-0cd49fd15648][Testing:1]]
;;; hermeneus-test.el --- Tests for Hermeneus -*- lexical-binding: t -*-
;; Testing:1 ends here

;; [[id:TKR:ac0367f5-9016-4139-acef-3183123bb0d6][Testing:1]]
(ert-deftest conv-test "Test Beta to Unicode conversion"
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
(ert-deftest match-test ()
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

;; Testing:1 ends here

;; [[id:TKR:6a32401f-f9e7-4e52-9c47-65cc722cb2ba][Testing:1]]

;; Testing:1 ends here

;; [[id:TKR:ba63b06f-cf08-4ff1-9b1f-eed5387a6a47][Testing:1]]

;; Testing:1 ends here

;; [[id:TKR:2f830a16-8963-4182-a998-c45bfa2401c1][Testing:1]]
(ert-deftest cts-test ()
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

;; [[id:TKR:96d2cc57-bf6d-4024-b4ee-d6bb2703763d][Testing:1]]
;;; hermeneus-test.el ends here
;; Testing:1 ends here
