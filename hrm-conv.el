(require 'cl-lib)
(require 'ucs-normalize)

(eval-when-compile
  (require 'rx)
  (require 'cl-macs)
  (require 'subr-x))

(defun hrm--regexp-bracket-quote (string)
  "Return STRING, regexp-quoted and, if necessary, in square brackets.
This exists for when a regexp being generated may need to match
one character or more than one character, depending on the length
of input STRING."
  (if (> (length string) 1)
      (concat "[" (regexp-quote string) "]")
    (regexp-quote string)))

(defmacro hrm--make-regexp-versions (def-form &rest string-vars)
  "Define regexp versions of a series of string variables.
Each string in STRING-VARS will be given a regexp version, suffixed
\"regexp\", which will match any character in the string.
DEF-FORM should be one of ‘defvar’, ‘defconst’, or ‘setq’."
  (declare (indent 1)
           (debug ([&or "defvar" "defconst" "setq"] &rest symbolp)))
  `(progn
     ,@(cl-loop for var in string-vars
              collect (list def-form (intern (concat (symbol-name var) "-regexp"))
                            (hrm--regexp-bracket-quote (symbol-value var))))))

;; For comparison, here is ‘hrm--greek-letters’:
;; αβγδεϝζηθιϳκλμνξοπρςτυφχψωΑΒΓΔΕϜΖΗΘΙͿΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ
(defconst hrm--beta-letters--standard
  "abgdevzhqijklmncoprstufxywABGDEVZHQIJKLMNCOPRSTUFXYW")

(defvar hrm--beta-letters--user hrm--beta-letters--standard)

;; (defconst hrm--greek-kbd-letters
;;   "abgde`zhuiĳklmnjoprwtyfxcvABGDE~ZHUIĲKLMNJOPRWTYFXCV")

;; (defvar hrm--beta-letters--user hrm--beta-letters--standard)
;; (defvar hrm--beta-output-letters hrm--greek-letters)

;; For comparison, here are the non-combining versions of the
;; characters in ‘hrm--greek-diacritics’:
;; ᾿῾ `´῀¨ͺˉ˘
(defconst hrm--beta-diacritics
  ")(\\/=+|_^")

(defconst hrm--beta-punctuation
  "':;") ; "’·;"

(defconst hrm--beta-all--standard
  (concat hrm--beta-punctuation hrm--beta-diacritics hrm--beta-letters--standard))

(hrm--make-regexp-versions defconst
  hrm--beta-diacritics hrm--beta-punctuation
  hrm--beta-all--standard)

(defvar hrm--beta-all--user hrm--beta-all--standard)
(defvar hrm--beta-all--user-regexp
  (hrm--regexp-bracket-quote hrm--beta-all--user))

(defvar hrm--greek-all
  (concat hrm--greek-punctuation hrm--greek-diacritics
          (apply #'string
                 (delete-dups (append (string-to-list hrm--greek-letters)
                                      (string-to-list hrm--all-sigmas))))))

(defun hrm--make-beta-hash-1 (input-letters output-letters hash)
  (when (stringp input-letters)
    (setq input-letters (mapcar #'char-to-string
                                (string-to-list input-letters))))
  (cl-loop for i in input-letters
           for o across output-letters
           if (eq (length i) 1)
           do (puthash (string-to-char i) o hash)
           else do (setq hash (hrm--make-beta-hash-1 i (make-string (length i) o) hash))
           finally return hash))

(cl-defun hrm--make-beta-hash (input-letters &optional
                                             (output-letters hrm--greek-letters)
                                             (hash (make-hash-table :size (length hrm--greek-letters)))
                                             &key
                                             (output-punctuation hrm--greek-punctuation)
                                             (output-diacritics hrm--greek-diacritics)
                                             (input-punctuation hrm--beta-punctuation)
                                             (input-diacritics hrm--beta-diacritics))
  "Make a hash table for translating INPUT-LETTERS to OUTPUT-LETTERS.
INPUT-LETTERS can be a string or a list of strings.
OUTPUT-LETTERS must be a string.

If INPUT-LETTERS is a string, then each letter in INPUT-LETTERS
will be used as a key in the resulting hash table, with the
corresponding letter in OUTPUT-LETTERS as the value.

If INPUT-LETTERS is a list of strings, then each character in
each string is interpreted as alternate keys for whichever
character has the positional index in OUTPUT-LETTERS that the
string has in INPUT-LETTERS (a many-to-one mapping). E.g., the
arguments '(\"ab\" \"c\" \"d\") and \"xyz\" would result in a
hash table mapping \"a\" to \"x\", \"b\" to \"x\", \"c\" to
\"y\", and \"d\" to \"z\".

Use HASH if you want to start from an existing hash-table rather
than make a new one."
  (setq hash (hrm--make-beta-hash-1 input-letters output-letters hash))
  (when (and input-punctuation input-diacritics output-punctuation output-diacritics)
    (cl-loop for i across (concat input-punctuation input-diacritics)
             for o across (concat output-punctuation output-diacritics)
             do (puthash i o hash)))
  hash)

(defvar hrm--beta-hash--standard (hrm--make-beta-hash hrm--beta-letters--standard))
(defvar hrm--beta-hash--user (copy-hash-table hrm--beta-hash--standard))

(cl-defun hrm-conv--set-beta-input-type (&optional sym (def hrm-beta-input-type f))
  "Setter function for the option `hrm-beta-input-type'."
  (unless sym
    (setq sym 'hrm-beta-input-type))

  (cond ((eq def 'beta)
         (setq hrm--beta-hash--user
               hrm--beta-hash--standard)
         (setq hrm--beta-letters--user
               hrm--beta-letters--standard))

        ;; Note that standard Greek keyboard layouts don’t have
        ;; a key for the digamma or yot; so, for the ‘greek-kbd’
        ;; setting, I kind of fudged things. The digamma is
        ;; moved to the backtick/tilde key, while the yot is
        ;; moved to ĳ. I don’t think many people have that key
        ;; on their keyboards, either, but as I can’t find a
        ;; single word in the LSJ that contains a yot, I think
        ;; it’s best to avoid clobbering potentially useful
        ;; inputs with it.
        ;; Once Hermeneus is sophisticated enough to need
        ;; support for typing a yot in Greek-keyboard style Beta
        ;; code, I will be happy to rethink this.
        ;; …And if you need that functionality, PLEASE take me
        ;; out to dinner.
        ;; —Tina
        ((eq def 'greek-kbd)
         (let ((greek-kbd-def
                '("a" "b" "g" "d" "e" "`"  "z" "h" "u" "i" "ĳ" "k" "l" "m"
                  "n" "j" "o" "p" "r" "sw" "t" "y" "f" "x" "c" "v"
                  "A" "B" "G" "D" "E" "~"  "Z" "H" "U" "I" "Ĳ" "K" "L" "M"
                  "N" "J" "O" "P" "R" "SW" "T" "Y" "F" "X" "C" "V")))
           (setq hrm--beta-hash--user
                 (hrm--make-beta-hash greek-kbd-def)
                 hrm--beta-letters-user
                 (apply #'concat greek-kbd-def))))

        ((and (listp def) (cl-every #'stringp def))
         (setq hrm--beta-hash--user
               (hrm--make-beta-hash def)
               hrm--beta-letters--user
               (apply #'concat def)))

        (t (error "Invalid definition for ‘hrm-beta-input-type’: %s" def)))

  (setq hrm--beta-all--user
        (concat hrm--beta-punctuation hrm--beta-diacritics hrm--beta-letters--user)
        hrm--beta-all--user-regexp
        (hrm--regexp-bracket-quote hrm--beta-all--user))

  ;; if definition is specified, set the variable itself
  (when f
    (set-default sym def)))

(defcustom hrm-beta-input-type 'beta
  "How to interpret Latin letters used to represent Greek words.
Only affects user input. The default is the standard “Beta code”
used for representing Greek words in Latin characters. “Greek
Keyboard” translates a standard QWERTY keyboard layout to a
standard Greek keyboard layout. Finally, with “Custom mapping,”
you can define your own style of Beta code. This is represented
with a list of strings, each corresponding to a Greek letter (use
the Customize interface to see which ones). Each string only
needs to be one character, but you can add more characters onto
the string if you want more than one key to enter the same
letter.

If setting this outside of Customize, be sure to run
‘hrm-conv--set-beta-input-type’ afterward."
  :type `(choice (const beta :tag "Beta code")
                 (const greek-kbd :tag "Greek keyboard")
                 (list :tag "Custom mapping"
                       ,@(cl-loop for cg across hrm--greek-letters
                                for cb across hrm--beta-letters--standard
                                for sg = (if (eq cg ?ς)
                                             "σ/ς"
                                           (concat "  " (char-to-string cg)))
                                for sb = (char-to-string cb)
                                collect (list 'string :tag sg :value sb))))
  :tag "Hermeneus — Beta code input type"
  :set 'hrm-conv--set-beta-input-type
  :group 'hermeneus)

(defun hrm--convert-string-by-hash (string hash)
  "Return STRING, translated according to HASH.
  HASH should be a hash table where the keys are characters and the
  values are characters or strings."
  (cl-loop for l across string
           for o = (or (gethash l hash) l)
           concat (cl-etypecase o
                    (string o)
                    (character (char-to-string o)))))

(defun hrm-conv--change-diacritics-placement (string)
  (let ((rx (rx (group "*")
                (group (one-or-more (any ")(/\\=+|—^")))
                (group letter))))
    (replace-regexp-in-string rx "\\1\\3\\2" string)))

(defun hrm-conv--dieresis-before-accent (string)
  (let ((rx (rx (group (any "/\\"))
                (group "+" ))))
    (replace-regexp-in-string rx "\\2\\1" string)))

(defun hrm-conv--capitalize-after-asterisk (string)
  "If STRING contains an asterisk, return STRING with no asterisk
and with the first letter after it capitalized. Otherwise, return
STRING. (This also happens when no letters appear anywhere
following the asterisk.)"
  (if-let ((astr-idx (string-match-p "\*" string))   ; “asterisk index”
           (capt-idx (string-match-p (rx word-start) ; where to capitalize
                                     string (1+ astr-idx))))
      (progn (setq string (concat (substring string 0 astr-idx)
                                  (substring string (1+ astr-idx) capt-idx)
                                  (char-to-string (upcase (elt string capt-idx)))
                                  (substring string (1+ capt-idx))))
             (hrm-conv--capitalize-after-asterisk string))
    string))

(defun hrm-conv--normalize-beta-diacritics (string)
  (thread-first string
    (hrm-conv--change-diacritics-placement)
    (hrm-conv--dieresis-before-accent)
    (hrm-conv--capitalize-after-asterisk)))

(defun hrm-conv--normalize-sigmas (string)
  "Returns a copy of STRING, but with sigmas normalized.
Sigmas which end a word will be replaced with “ς”, while other sigmas
will be replaced with “σ”."
  ;; This function used to look like this:
  ;;
  ;; (replace-regexp-in-string (rx "ς" (not word-boundary)) "σ" string))
  ;;
  ;; An elaborate rewrite was necessary because Emacs’s regexp engine
  ;; now sees “σ” and “ς” as equivalent, which confuses the hell out
  ;; of ‘replace-regexp-in-string’.
  (let ((max (length string))
        done i substrings sigma)
    (while (not done)
      (if-let ((sigma-idx (string-match-p "ς" string i)) ; the actual test
               (next-idx (1+ sigma-idx))
               (new-substring (substring string (or i 0) sigma-idx)))
          ;; sigma is present
          (progn
            (setq sigma
                  ;; does it end a word?
                  (if (or (and (eq next-idx max) (setq done t))
                          (not (eq (char-syntax (elt string next-idx)) ?w)))
                      "ς" ; yes
                    "σ")) ; no
            (setq substrings
                  (cons sigma (cons new-substring substrings)))
            (setq i next-idx))
        ;; no more sigmas
        (if (not i) ; ‘i’ will be nil if the string had no sigmas at all
            (setq done string)
          (push (substring string i max) substrings)
          (setq done t))))
    (if (stringp done)
        done
      (apply #'concat (nreverse substrings)))))

(defun hrm-beta-to-unicode (string &optional input-p match-p)
  "Return STRING converted from Beta code to Unicode.
INPUT-P is whether or not the string should be interpreted as
user input. (The difference is that user input should be read
according to the option ‘hrm-conv-beta-input-type’; otherwise, it
should be read as standard Beta code, as used in the XML LSJ.)
MATCH-P should be non-nil when converting a string to be used
solely for matching (like in the function ‘hrm--re-builder’), in
which case sigma normalization is unnecessary."
  (setq string
        (thread-first string
          (hrm-conv--normalize-beta-diacritics)
          (hrm--convert-string-by-hash (if input-p
                                           hrm--beta-hash--user
                                         hrm--beta-hash--standard))
          (ucs-normalize-NFC-string)))
  (if match-p
      string
    (hrm-conv--normalize-sigmas string)))

(provide 'hrm-conv)

  ;;; hrm-conv.el ends here
