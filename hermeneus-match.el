;;; hermeneus-match.el --- -*- lexical-binding: t -*-

(require 'ucs-normalize)

(eval-when-compile (require 'rx)
                   (require 'cl-macs)
                   (require 'subr-x))

(require 'hermeneus-conv)

(defvar hermeneus--greek-punctuation)

(hermeneus--make-regexp-versions defconst
  hermeneus--lowercase-sigmas hermeneus--uppercase-sigmas hermeneus--all-sigmas)

(defun hermeneus-remove-diacritics (string)
  "Return STRING, but with all diacritics removed."
  (replace-regexp-in-string (rx (category combining-diacritic)) ""
                            (ucs-normalize-NFD-string string)))

(defun hermeneus-normalize-greek-char (char)
  (thread-first char
    (char-to-string)
    (hermeneus-remove-diacritics)
    (string-to-char)))

(cl-defun hermeneus--get-letter-variants (char)
  (when (stringp char)
    (setq char (string-to-char char)))
  ;; if it’s a sigma, return all sigmas of that case
  (if (memq char (string-to-list hermeneus--all-sigmas))
      (if (memq char (string-to-list hermeneus--lowercase-sigmas))
          hermeneus--lowercase-sigmas
        hermeneus--uppercase-sigmas)
    ;; otherwise, look up variants in ‘hermeneus--greek-unicode-all’
    (cl-loop for c across hermeneus--greek-unicode-all
             if (eq (hermeneus-normalize-greek-char char)
                    (hermeneus-normalize-greek-char c))
             concat (char-to-string c))))

(defvar hermeneus--letter-variant-hash (make-hash-table
                                        :size (length hermeneus--greek-all)))
(defvar hermeneus--letter-variant-hash-case-folded (make-hash-table
                                                    :size (length hermeneus--greek-all)))

(defun hermeneus--make-letter-variant-hashes ()
  (cl-loop with letters = (thread-first hermeneus--greek-letters
                            (concat hermeneus--all-sigmas)
                            (string-to-list)
                            (delete-dups))
           for l in letters
           for ld = (downcase l)
           for lu = (upcase l)
           for variants = (hermeneus--get-letter-variants l)
           for casefold-variants = (if (eq ld lu)
                                       variants
                                     (concat (hermeneus--get-letter-variants ld)
                                             (hermeneus--get-letter-variants lu)))
           do (puthash l (hermeneus--regexp-bracket-quote variants)
                       hermeneus--letter-variant-hash)
              (puthash l (hermeneus--regexp-bracket-quote casefold-variants)
                       hermeneus--letter-variant-hash-case-folded)))

(hermeneus--make-letter-variant-hashes)

(defvar hermeneus--case-fold-hash (make-hash-table :size (length hermeneus--greek-unicode-all)))

(defun hermeneus--make-case-fold-hash ()
  (cl-loop for l across hermeneus--greek-unicode-all
           for lu = (upcase l)
           for ld = (downcase l)
           if (memq l (string-to-list hermeneus--all-sigmas))
           do (puthash l hermeneus--all-sigmas-regexp hermeneus--case-fold-hash)
           else if (eq ld lu)
           do (puthash l l hermeneus--case-fold-hash)
           else do (puthash l (format "[%c%c]" ld lu)
                            hermeneus--case-fold-hash)))

(hermeneus--make-case-fold-hash)

(defvar hermeneus--sigma-hash (make-hash-table :size (length hermeneus--all-sigmas)))

(defun hermeneus--make-sigma-hash ()
  (cl-loop for l across hermeneus--lowercase-sigmas
           do (puthash l hermeneus--lowercase-sigmas-regexp hermeneus--sigma-hash))
  (cl-loop for l across hermeneus--uppercase-sigmas
           do (puthash l hermeneus--uppercase-sigmas-regexp hermeneus--sigma-hash)))

(hermeneus--make-sigma-hash)

(defun hermeneus--re-builder (string)
  ;; Decompose the string into letters and combining diacriticals, and
  ;; translate any Beta code to Unicode
  (setq string (ucs-normalize-NFD-string string))
  (when (string-match-p hermeneus--beta-all--user-regexp string)
    (setq string (thread-first string
                   (hermeneus-conv--normalize-beta-diacritics)
                   (hermeneus-beta-to-unicode t t))))
  ;; Figure out if we need to fold accents or case
  (let* ((fold-accents-p (not (string-match-p
                               (rx (category combining-diacritic))
                               string)))
         (fold-case-p (or (and case-fold-search
                               (not (eq case-fold-search 'auto)))
                          (and (eq case-fold-search 'auto)
                               (let ((case-fold-search nil))
                                 (not (string-match-p (rx upper) string))))))
         ;; Pick a hash to use for the translation
         (hash (if fold-accents-p
                   ;; Yes, folding accents
                   (if fold-case-p
                       hermeneus--letter-variant-hash-case-folded
                     hermeneus--letter-variant-hash)
                 ;; No, not folding accents
                 (setq string (ucs-normalize-NFC-string string)) ; recompose accents
                 (if fold-case-p
                     hermeneus--case-fold-hash
                   hermeneus--sigma-hash))))
    (hermeneus--convert-string-by-hash string hash)))

(defun hermeneus--re-matcher (regexp candidates)
  "Return all strings in CANDIDATES that match REGEXP.
Strings where the regexp matches at the beginning will be listed earlier
in the result than strings where the regexp matches elsewhere."
  (if (string-empty-p regexp)
      candidates
    (let (list-1 list-2)
      (dolist (c (reverse candidates))
        (when (string-match-p regexp c)
          (if (string-match-p (concat "^" regexp) c)
              (push c list-1)
            (push c list-2))))
      (append list-1 list-2))))

(provide 'hermeneus-match)

;;; hermeneus-match.el ends here
