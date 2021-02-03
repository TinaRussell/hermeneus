(require 'eieio-core)
(require 'cl-preloaded)
(require 'eieio)
(require 'custom)
(require 'derived)

(eval-when-compile (require 'subr-x)
                   (require 'rx)
                   (require 'cl-macs))

(defconst hrm--greek-letters "αβγδεϝζηθιϳκλμνξοπρςτυφχψωΑΒΓΔΕϜΖΗΘΙͿΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ"
  "Every letter in the Greek alphabet, including the digamma and yot.")

(defconst hrm--greek-diacritics "\u0313\u0314\u0300\u0301\u0342\u0308\u0345\u0304\u0306"
  "Every combining diacritic relevant to Ancient Greek.")

(defconst hrm--greek-punctuation "’·;")

;; This isn’t actually used anywhere. I don’t remember why I made it.
;; But it’s nice, so it stays.
(defconst hrm--greek-letter-names
  (list "alpha"  "beta"   "gamma" "delta" "epsilon" "digamma" "zeta"
          "eta" "theta"    "iota"   "yot"   "kappa"  "lambda"   "mu"
           "nu"    "xi" "omicron"    "pi"     "rho"   "sigma"  "tau"
      "upsilon"   "phi"     "chi"   "psi"   "omega"))

(defconst hrm--greek-unicode-all
  (cl-loop for v being the hash-values of (ucs-names)
           for v = (char-to-string v)
           if (string-match-p (rx (category greek)) v)
           concat v))

(defconst hrm--lowercase-sigmas "σςϲͻͼͽ")
(defconst hrm--uppercase-sigmas "ΣϹϽϾϿ")

;; Oops! ALL sigmas
(defconst hrm--all-sigmas (concat hrm--lowercase-sigmas
                                  hrm--uppercase-sigmas)
  "Every sigma. All of them.
You need a lowercase word-ending sigma? Consider it done. How
about a capital reverse dotted lunate sigma? We’ve got you
covered. Is this madness, you ask? Madness? THIS IS SIGMA!")

(defconst hrm--git-lsj-dir
  "https://raw.githubusercontent.com/PerseusDL/lexica/master/CTS_XML_TEI/perseus/pdllex/grc/lsj/"
  "Location of the LSJ within the PerseusDL “lexica” repository.")

(defun hrm-alist-to-local-vars (alist &optional prefix)
  "Convert each key in an alist to a local variable.
Each variable will have the name and value of the relevant key.
If PREFIX is a string, it will be added to the beginning of each
variable name (with a hyphen in between)."
  (let (rtn)
    (dolist (a alist (nreverse rtn))
      (let* ((var-sym (if (stringp prefix)
                          (intern (concat prefix "-" (symbol-name (car a))))
                        (car a)))
             (var (make-local-variable var-sym)))
        (set var (cdr a))
        (push var rtn)))))

(defun hrm--make-keyword (symbol)
  (make-symbol (concat ":" (symbol-name symbol))))

(defun hrm--get-slot-default-value (class slot)
  "Return the default value of SLOT in CLASS."
  (thread-first (cl-loop for slot in (eieio-class-slots class)
                         if (eq (cl--slot-descriptor-name slot) 'object-name)
                         return slot)
    (cl--slot-descriptor-initform)
    (eieio-default-eval-maybe)))

(defun hrm--trim-string-extra (string)
  "Trim a string more aggressively than the function ‘string-trim’.
Returns STRING, with whitespace and punctuation characters found
at each end removed."
  (let ((trim (rx (one-or-more (any blank punctuation ?\n)))))
    (string-trim string trim trim)))

(defgroup hermeneus nil
  "Options for Hermeneus, the Ancient Greek word utility."
  :tag "Hermeneus"
  :group 'applications)

(defgroup hrm-faces nil
  "Faces used in Hermeneus, the Ancient Greek word utility."
  :tag "Hermeneus faces"
  :group 'hermeneus)

(defcustom hrm-scan-entry-functions nil
  "Functions called by ‘hrm-scan-xml’ for every XML element
in the lexicon. Each function is run with two arguments: the
word-object corresponding to the entry, and the DOM parsed
from the XML element itself."
  :type 'hook
  :group 'hermeneus)

(define-derived-mode hermeneus-mode special-mode "Hermeneus")

(define-key hermeneus-mode-map "g" 'hrm-buffer-update)

(require 'hrm-conv)
(require 'hrm-match)
(require 'hrm-xml)
(require 'hrm-completion)
(require 'hrm-storage)
(require 'hrm-cts)
(require 'hrm-tags)
(require 'hrm-render)

(provide 'hermeneus)

;; hermeneus.el ends here
