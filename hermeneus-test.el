;;; hermeneus-test.el --- Tests for Hermeneus -*- lexical-binding: t -*-

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







(ert-deftest cts-test ()
  "Test the Canonical Test Services functionality."
  (should (equal (hermeneus-urn-to-base "urn:cts:greekLit:tlg0020.tlg001.perseus-grc1:195")
                 "urn:cts:greekLit:tlg0020.tlg001.perseus-grc1"))
  (should (equal (hermeneus-urn-to-work "urn:cts:greekLit:tlg0020.tlg001.perseus-grc1:195")
                 "urn:cts:greekLit:tlg0020.tlg001"))
  (should (equal (hermeneus-urn-to-url "urn:cts:greekLit:tlg0020.tlg001.perseus-grc1:195")
                 "http://data.perseus.org/catalog/urn:cts:greekLit:tlg0020.tlg001.perseus-grc1")))





;;; hermeneus-test.el ends here
