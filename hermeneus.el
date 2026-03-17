;;; hermeneus.el --- Ancient Greek word lookup tool -*- lexical-binding: t -*-

;; Author: Tina Russell
;; Maintainer: Tina Russell
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (anaphora "1.0.4"))
;; Homepage: https://github.com/TinaRussell/hermeneus
;; Keywords: greek language reference


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


;;; Commentary:

;; This is Hermeneus, the Ancient Greek word system and lookup tool.
;; Once this package is loaded, use the command ‘describe-greek-word’
;; to look up a word in Ancient Greek. The ‘ivy’ package is
;; recommended (see option ‘hermeneus-use-ivy’).

;;; Code:

(require 'eieio-core)
(require 'cl-preloaded)
(require 'eieio)
(require 'custom)
(require 'derived)

(eval-when-compile (require 'subr-x)
                   (require 'rx)
                   (require 'cl-macs))

(defconst hermeneus--greek-letters "αβγδεϝζηθιϳκλμνξοπρςτυφχψωΑΒΓΔΕϜΖΗΘΙͿΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ"
  "Every letter in the Greek alphabet, including the digamma and yot.")

(defconst hermeneus--greek-diacritics "\u0313\u0314\u0300\u0301\u0342\u0308\u0345\u0304\u0306"
  "Every combining diacritic relevant to Ancient Greek.")

(defconst hermeneus--greek-punctuation "’·;")

;; This isn’t actually used anywhere. I don’t remember why I made it.
;; But it’s nice, so it stays.
(defconst hermeneus--greek-letter-names
  (list "alpha"  "beta"   "gamma" "delta" "epsilon" "digamma" "zeta"
          "eta" "theta"    "iota"   "yot"   "kappa"  "lambda"   "mu"
           "nu"    "xi" "omicron"    "pi"     "rho"   "sigma"  "tau"
      "upsilon"   "phi"     "chi"   "psi"   "omega"))

(defconst hermeneus--greek-unicode-all
  (cl-loop for v being the hash-values of (ucs-names)
           for v = (char-to-string v)
           if (string-match-p (rx (category greek)) v)
           concat v))

(defconst hermeneus--lowercase-sigmas "σςϲͻͼͽ")
(defconst hermeneus--uppercase-sigmas "ΣϹϽϾϿ")

(defconst hermeneus--all-sigmas (concat hermeneus--lowercase-sigmas
                                  hermeneus--uppercase-sigmas)
  "Every sigma. All of them.
You need a lowercase word-ending sigma? Consider it done. How
about a capital reverse dotted lunate sigma? We’ve got you
covered. Is this madness, you ask? Madness? THIS IS SIGMA!")

(defconst hermeneus--git-lsj-dir
  "https://raw.githubusercontent.com/PerseusDL/lexica/master/CTS_XML_TEI/perseus/pdllex/grc/lsj/"
  "Location of the LSJ within the PerseusDL “lexica” repository.")

(defun hermeneus-alist-to-local-vars (alist &optional prefix)
  "Convert each key in alist ALIST to a local variable.
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

(defun hermeneus--make-keyword (symbol)
  "Make a keyword with the same name as SYMBOL (but with a colon)."
  (make-symbol (concat ":" (symbol-name symbol))))

(defun hermeneus--get-slot-default-value (class slot)
  "Return the default value of SLOT in CLASS.
SLOT should be given as a symbol. Signals an error if CLASS does
not contain a slot named SLOT."
  (if-let ((slot-actual
            (cl-loop for s in (eieio-class-slots class)
                     if (eq (cl--slot-descriptor-name s) slot)
                     return s)))
      (thread-first slot-actual
        (cl--slot-descriptor-initform)
        (eieio-default-eval-maybe))
    (error "Class %s does not appear to contain slot ‘%s’" class slot)))

(defun hermeneus--trim-string-extra (string)
  "Trim a string more aggressively than the function ‘string-trim’.
Returns STRING, with whitespace and punctuation characters found
at each end removed."
  (let ((trim (rx (one-or-more (any blank punctuation ?\n)))))
    (string-trim string trim trim)))

(defgroup hermeneus nil
  "Options for Hermeneus, the Ancient Greek word utility."
  :tag "Hermeneus"
  :group 'applications
  :prefix "hermeneus-")

(defgroup hermeneus-faces nil
  "Faces used in Hermeneus, the Ancient Greek word utility."
  :tag "Hermeneus faces"
  :group 'hermeneus)

(defcustom hermeneus-scan-entry-functions nil
  "Functions called by ‘hermeneus-scan-xml’ for every XML element
in the lexicon. Each function is run with two arguments: the
word-object corresponding to the entry, and the DOM parsed
from the XML element itself."
  :type 'hook
  :group 'hermeneus)

(define-derived-mode hermeneus-mode special-mode "Hermeneus")

(define-key hermeneus-mode-map "g" 'hermeneus-buffer-update)

(require 'hermeneus-conv)
(require 'hermeneus-match)
(require 'hermeneus-xml)
(require 'hermeneus-completion)
(require 'hermeneus-storage)
(require 'hermeneus-cts)
(require 'hermeneus-tags)
(require 'hermeneus-render)

(provide 'hermeneus)

;;; hermeneus.el ends here
