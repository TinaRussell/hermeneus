;;; hermeneus-completion.el --- -*- lexical-binding: t -*-

;; [[id:TKR:a33012b6-462c-44fd-b63f-65f07969227c][Dependencies:1]]
(require 'cl-extra)
(require 'custom)
(require 'eieio)
(require 'ucs-normalize)
(require 'rx)

(eval-when-compile (require 'cl-macs)
                   (require 'subr-x))

(require 'hermeneus-conv)
(require 'hermeneus-match)
(require 'hermeneus-xml)

(defvar hermeneus--greek-punctuation)
;; Dependencies:1 ends here

;; [[id:TKR:7f0bab31-c461-44f0-898d-a80deeefd174][Variables:1]]
(defcustom hermeneus-use-ivy (let ((libs '(ivy counsel)))
                         (or (cl-every 'featurep libs)
                             (cl-every 'package-installed-p libs)))
  "Whether to use the ‘ivy’ package for ‘describe-greek-word’.
This allows two important features. The first is matching by Beta code:
if you type in Beta code (e.g. “i(ero/doulos” instead of “ἱερόδουλος”),
it will match as though you typed the Greek Unicode equivalent. (See
option ‘hermeneus-beta-input-type’ if you want to customize the type of
Beta code used for this.) The second is diacritic-agnostic matching: if
you type Greek with no diacritics into the ‘describe-greek-word’ prompt,
it will match any combination of diacritics on the same sequence of
letters. This works whether you’re typing in Greek Unicode or in Beta
code: so, either “etaira” or “εταιρα” will match “ἑταίρα”.

This option has no effect if Ivy is not installed. If Ivy is installed,
but this option is turned off (‘nil’), then the Ivy version of
‘describe-greek-word’ is still available as the command
‘counsel-greek-word’."
  :type 'boolean
  :group 'hermeneus)
;; Variables:1 ends here

;; [[id:TKR:4864efa6-7652-4504-b780-f06f24283984][Describe Greek word:1]]
;;;###autoload
(defun describe-greek-word (word)
  (interactive
   (list
    (let ((entries (hermeneus-get-entries hermeneus-lsj)))
      (if (and hermeneus-use-ivy (fboundp 'ivy-read))
          (counsel-greek-word "Look up Greek word: " hermeneus-lsj)
        (let ((default (hermeneus-greek-word-at-point)))
          (completing-read (format "Look up Greek word%s: "
                                   (if default
                                       (format " (default: %s)" default)
                                     ""))
                           entries nil t nil nil default))))))
  (unless (and hermeneus-use-ivy (fboundp 'ivy-read))
    (hermeneus--display-word-buffer word)))
;; Describe Greek word:1 ends here

;; [[id:TKR:e488b337-55e7-4543-ae83-dfdedc268b8c][Read word:1]]
(cl-defun counsel-greek-word (&optional (prompt "Look up Greek word:")
                                        (lexicon hermeneus-lsj) &rest kwargs)
  "Read a Greek word from the LSJ, with Ivy completion.
COLLECTION should be a ‘hermeneus-lexicon’ object or a hash-table, and
defaults to the value of ‘hermeneus-lsj’. Any other arguments should be
keyword arguments, which are passed to ‘ivy-read’."
  (unless (fboundp 'ivy-read)
    (error "Ivy must be installed before using ‘counsel-greek-word’"))
  (let ((collection
         (cond ((hermeneus-lexicon-p lexicon)
                (hermeneus-get-entries lexicon))
               ((hash-table-p lexicon)
                lexicon)
               (t (error "Not a hermeneus-lexicon object or hash table: %s" lexicon)))))
    (cl-flet ((kw-put (prop val)
                      (unless (plist-member kwargs prop)
                        (cl-callf plist-put kwargs prop val))))
      (kw-put :action #'hermeneus--display-word-buffer)
      (kw-put :re-builder #'hermeneus--re-builder)
      (kw-put :matcher #'hermeneus--re-matcher)
      (awhen (hermeneus-greek-word-at-point)
        (kw-put :preselect it))
      (apply 'ivy-read prompt collection kwargs))))
;; Read word:1 ends here

;; [[id:TKR:9804b1a7-dae0-4425-b671-2cdc3b604017][Get Greek letter equivalents:1]]
(defun hermeneus--fold-case (string)
  (cl-loop for l across (regexp-quote string)
           if (memq l (string-to-list hermeneus--all-sigmas))
           concat (format "[%s]" hermeneus--all-sigmas)
           else
           concat (let ((upr (upcase l))
                        (lwr (downcase l)))
                    (if (eq upr lwr)
                        (char-to-string l)
                      (format "[%c%c]" upr lwr)))))
;; Get Greek letter equivalents:1 ends here

;; [[id:TKR:3d941c67-20eb-4ad6-9653-bc545e109827][Greek word at point:1]]
(defun hermeneus--bounds-of-chars (chars)
  "Skip CHARS backwards and forwards, return a cons of each point.
CHARS is a string containing the characters to skip over. If point is
not adjacent to any characters in CHARS, return nil."
  (let ((rtn (cons
              (save-excursion (skip-chars-backward chars)
                              (point))
              (save-excursion (skip-chars-forward chars)
                              (point)))))
    (unless (eql (car rtn) (cdr rtn))
      rtn)))

(defun hermeneus-bounds-of-greek-word-at-point ()
  (or (hermeneus--bounds-of-chars (concat hermeneus--greek-unicode-all
                                    hermeneus--greek-diacritics))
      (hermeneus--bounds-of-chars (concat hermeneus--beta-letters--user ; TODO should check user and standard variants
                                    hermeneus--beta-diacritics "*"))))

(defun hermeneus-greek-word-at-point ()
  (when-let ((bounds (hermeneus-bounds-of-greek-word-at-point))
             (word (buffer-substring-no-properties (car bounds) (cdr bounds)))
             (obj (hermeneus--string-to-object word hermeneus-lsj)))
    (oref obj key)))

(put (intern "greek-word") 'bounds-of-thing-at-point 'hermeneus-bounds-of-greek-word-at-point)
;; Greek word at point:1 ends here

;; [[id:TKR:03304dde-533f-41bb-937b-99e3f31ef2f1][Look up Greek word noninteractively:1]]
(cl-defun hermeneus--string-to-object (string &optional (lexicon hermeneus-lsj))
  "Retrieve the word-object in LEXICON corresponding to STRING.
The function ‘hermeneus--fuzzy-search’ is used when there isn’t an exact
match. If no result is found, return nil."
  (unless (and (stringp string) (hermeneus-lexicon-p lexicon))
    (error "Incorrect arguments for ‘hermeneus--string-to-object’: %s %s"
           string lexicon))
  (or (gethash string (hermeneus-get-entries lexicon))
      (hermeneus--fuzzy-search string)
      (hermeneus--fuzzy-search (string-trim string))
      (hermeneus--fuzzy-search (hermeneus--trim-string-extra string))))

(cl-defun hermeneus--fuzzy-search (string &optional (lexicon hermeneus-lsj))
  "Look up the word STRING in LEXICON (which defaults to the LSJ).
The functions ‘hermeneus--re-builder’ and ‘hermeneus--re-matcher’ are used to
provide fuzzy-matching. Returns a word-object."
  (let* ((hermeneus-beta-input-type 'beta)
         (re (hermeneus--re-builder string))
         (entries (hermeneus-get-entries lexicon))
         (matches (hermeneus--re-matcher re (hash-table-keys entries))))
    (when matches
      (gethash (car matches) entries))))
;; Look up Greek word noninteractively:1 ends here

;; [[id:TKR:bd16e1ce-fd2c-4c49-9a73-1d2038210079][Display word buffer:1]]
(cl-defun hermeneus--display-word-buffer (word &optional (lexicon hermeneus-lsj))
  "Display WORD, a string or word-object, from LEXICON (default: LSJ)."
  (unless (hermeneus-word-p word)
    (if (stringp word)
        (setq word (hermeneus--string-to-object word lexicon))
      (error "Argument is neither a ‘hermeneus-word’ object nor a string: %s"
             word)))
  (when word
    (prog1 (hermeneus--switch-buffer
            (hermeneus--word-buffer word))
      (hermeneus--clear-old-buffers))))
;; Display word buffer:1 ends here

(provide 'hermeneus-completion)

;;; hermeneus-completion.el ends here
