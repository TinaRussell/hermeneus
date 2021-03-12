;;; hrm-match.el --- -*- lexical-binding: t -*-

(require 'ucs-normalize)

(eval-when-compile (require 'rx)
                   (require 'cl-macs)
                   (require 'subr-x))

(require 'hrm-conv)

(defvar hrm--greek-punctuation)

(hrm--make-regexp-versions defconst
  hrm--lowercase-sigmas hrm--uppercase-sigmas hrm--all-sigmas)

(defun hrm-remove-diacritics (string)
  "Return STRING, but with all diacritics removed."
  (replace-regexp-in-string (rx (category combining-diacritic)) ""
                            (ucs-normalize-NFD-string string)))

(defun hrm-normalize-greek-char (char)
  (thread-first char
    (char-to-string)
    (hrm-remove-diacritics)
    (string-to-char)))

(cl-defun hrm--get-letter-variants (char)
  (when (stringp char)
    (setq char (string-to-char char)))
  ;; if it’s a sigma, return all sigmas of that case
  (if (memq char (string-to-list hrm--all-sigmas))
      (if (memq char (string-to-list hrm--lowercase-sigmas))
          hrm--lowercase-sigmas
        hrm--uppercase-sigmas)
    ;; otherwise, look up variants in ‘hrm--greek-unicode-all’
    (cl-loop for c across hrm--greek-unicode-all
             if (eq (hrm-normalize-greek-char char)
                    (hrm-normalize-greek-char c))
             concat (char-to-string c))))

(defvar hrm--letter-variant-hash (make-hash-table :size (length hrm--greek-all)))
(defvar hrm--letter-variant-hash-case-folded (make-hash-table :size (length hrm--greek-all)))

(defun hrm--make-letter-variant-hashes ()
  (cl-loop with letters = (thread-first hrm--greek-letters
                            (concat hrm--all-sigmas)
                            (string-to-list)
                            (delete-dups))
           for l in letters
           for ld = (downcase l)
           for lu = (upcase l)
           for variants = (hrm--get-letter-variants l)
           for casefold-variants = (if (eq ld lu)
                                       variants
                                     (concat (hrm--get-letter-variants ld)
                                             (hrm--get-letter-variants lu)))
           do (puthash l (hrm--regexp-bracket-quote variants)
                       hrm--letter-variant-hash)
              (puthash l (hrm--regexp-bracket-quote casefold-variants)
                       hrm--letter-variant-hash-case-folded)))

(hrm--make-letter-variant-hashes)

(defvar hrm--case-fold-hash (make-hash-table :size (length hrm--greek-unicode-all)))

(defun hrm--make-case-fold-hash ()
  (cl-loop for l across hrm--greek-unicode-all
           for lu = (upcase l)
           for ld = (downcase l)
           if (memq l (string-to-list hrm--all-sigmas))
           do (puthash l hrm--all-sigmas-regexp hrm--case-fold-hash)
           else if (eq ld lu)
           do (puthash l l hrm--case-fold-hash)
           else do (puthash l (format "[%c%c]" ld lu)
                            hrm--case-fold-hash)))

(hrm--make-case-fold-hash)

(defvar hrm--sigma-hash (make-hash-table :size (length hrm--all-sigmas)))

(defun hrm--make-sigma-hash ()
  (cl-loop for l across hrm--lowercase-sigmas
           do (puthash l hrm--lowercase-sigmas-regexp hrm--sigma-hash))
  (cl-loop for l across hrm--uppercase-sigmas
           do (puthash l hrm--uppercase-sigmas-regexp hrm--sigma-hash)))

(hrm--make-sigma-hash)

(defun hrm--re-builder (string)
  ;; Decompose the string into letters and combining diacriticals, and
  ;; translate any Beta code to Unicode
  (setq string (ucs-normalize-NFD-string string))
  (when (string-match-p hrm--beta-all--user-regexp string)
    (setq string (thread-first string
                   (hrm-conv--normalize-beta-diacritics)
                   (hrm-beta-to-unicode t t))))
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
                       hrm--letter-variant-hash-case-folded
                     hrm--letter-variant-hash)
                 ;; No, not folding accents
                 (setq string (ucs-normalize-NFC-string string)) ; recompose accents
                 (if fold-case-p
                     hrm--case-fold-hash
                   hrm--sigma-hash))))
    (hrm--convert-string-by-hash string hash)))

(defun hrm--re-matcher (regexp candidates)
  "Return all strings in CANDIDATES that match REGEXP.
Strings where the regexp matches at the beginning will be listed
earlier in the result than strings where the regexp matches
elsewhere."
  (if (string-empty-p regexp)
      candidates
    (let (list-1 list-2)
      (dolist (c (reverse candidates))
        (when (string-match-p regexp c)
          (if (string-match-p (concat "^" regexp) c)
              (push c list-1)
            (push c list-2))))
      (append list-1 list-2))))

(provide 'hrm-match)

;;; hrm-match.el ends here
