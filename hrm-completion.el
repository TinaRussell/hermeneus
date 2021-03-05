;;; hrm-completion.el --- -*- lexical-binding: t -*-

(require 'cl-extra)
(require 'custom)
(require 'eieio)
(require 'ucs-normalize)
(require 'rx)

(eval-when-compile (require 'cl-macs)
                   (require 'subr-x))

(require 'hrm-conv)
(require 'hrm-match)

(defcustom hrm-use-ivy (let ((libs '(ivy counsel)))
                         (or (cl-every 'featurep libs)
                             (cl-every 'package-installed-p libs)))
  "Whether to use the ‘ivy’ package for ‘describe-greek-word’.
This allows two important features. The first is matching by Beta
code: if you type in Beta code (i.e. “i(ero/doulos” instead of
“ἱερόδουλος”), it will match as though you typed the Greek
Unicode equivalent. The second is diacritic-agnostic matching: if
you type Greek with no diacritics into the ‘describe-greek-word’
prompt, it will match any combination of diacritics on the same
sequence of letters. This works whether you’re typing in Greek
Unicode or in Beta code: so, either “etaira” or “εταιρα” will
match “ἑταίρα”.

This option has no effect if Ivy is not installed. If Ivy is
installed, but this option is turned off (‘nil’), then the Ivy
version of ‘describe-greek-word’ is still available as the
command ‘counsel-greek-word’."
  :type 'boolean
  :group 'hermeneus)

;;;###autoload
(defun describe-greek-word (word)
  (interactive
   (list
    (let ((entries (oref hrm-lsj entries)))
      (if (and hrm-use-ivy (fboundp 'ivy-read))
          (counsel-greek-word "Look up Greek word: " hrm-lsj)
        (let ((default (hrm-greek-word-at-point)))
          (completing-read (format "Look up Greek word%s: "
                                   (if default
                                       (format " (default: %s)" default)
                                     ""))
                           entries nil t nil nil default))))))
  (unless (and hrm-use-ivy (fboundp 'ivy-read))
    (hrm--display-word-buffer word)))

(cl-defun counsel-greek-word (&optional (prompt "Look up Greek word:")
                                        (lexicon hrm-lsj) &rest kwargs)
  "Read a Greek word from the LSJ, with Ivy completion.
COLLECTION should be a ‘hrm-lexicon’ object or a hash-table, and
defaults to the value of ‘hrm-lsj’. Any other arguments should be
keyword arguments, which are passed to ‘ivy-read’."
  (unless (fboundp 'ivy-read)
    (error "Ivy must be installed before using ‘counsel-greek-word’"))
  (let ((collection
         (cond ((hrm-lexicon-p lexicon)
                (oref lexicon entries))
               ((hash-table-p lexicon)
                lexicon)
               (t (error "Not a hrm-lexicon object or hash table: %s" lexicon)))))
    (cl-flet ((kw-put (prop val)
                      (unless (plist-member kwargs prop)
                        (cl-callf plist-put kwargs prop val))))
      (kw-put :action #'hrm--display-word-buffer)
      (kw-put :re-builder #'hrm--re-builder)
      (kw-put :matcher #'hrm--re-matcher)
      (awhen (hrm-greek-word-at-point)
        (kw-put :preselect it))
      (apply 'ivy-read prompt collection kwargs))))

(defun hrm--fold-case (string)
  (cl-loop for l across (regexp-quote string)
           if (memq l (string-to-list hrm--all-sigmas))
           concat (format "[%s]" hrm--all-sigmas)
           else
           concat (let ((upr (upcase l))
                        (lwr (downcase l)))
                    (if (eq upr lwr)
                        (char-to-string l)
                      (format "[%c%c]" upr lwr)))))

(defun hrm--bounds-of-chars (chars)
  "Skip CHARS backwards and forwards, return a cons of each point.
CHARS is a string containing the characters to skip over. If
point is not adjacent to any characters in CHARS, return nil."
  (let ((rtn (cons
              (save-excursion (skip-chars-backward chars)
                              (point))
              (save-excursion (skip-chars-forward chars)
                              (point)))))
    (unless (eql (car rtn) (cdr rtn))
      rtn)))

(defun hrm-bounds-of-greek-word-at-point ()
  (or (hrm--bounds-of-chars (concat hrm--greek-unicode-all
                                    hrm--greek-diacritics))
      (hrm--bounds-of-chars (concat hrm--beta-letters--user ; TODO should check user and standard variants
                                    hrm--beta-diacritics "*"))))

(defun hrm-greek-word-at-point ()
  (when-let ((bounds (hrm-bounds-of-greek-word-at-point))
             (word (buffer-substring-no-properties (car bounds) (cdr bounds)))
             (obj (hrm--string-to-object word hrm-lsj)))
    (oref obj key)))

(put (intern "greek-word") 'bounds-of-thing-at-point 'hrm-bounds-of-greek-word-at-point)

(cl-defun hrm--string-to-object (string &optional (lexicon hrm-lsj))
  "Retrieve the word-object in LEXICON corresponding to STRING.
The function ‘hrm--fuzzy-search’ is used when there isn’t an
exact match. If no result is found, return nil."
  (unless (and (stringp string) (hrm-lexicon-p lexicon))
    (error "Incorrect arguments for ‘hrm--string-to-object’: %s %s"
           string lexicon))
  (or (gethash string (oref lexicon entries))
      (hrm--fuzzy-search string)
      (hrm--fuzzy-search (string-trim string))
      (hrm--fuzzy-search (hrm--trim-string-extra string))))

(cl-defun hrm--fuzzy-search (string &optional (lexicon hrm-lsj))
  "Look up the word STRING in LEXICON (which defaults to the LSJ).
The functions ‘hrm--re-builder’ and ‘hrm--re-matcher’ are used to
provide fuzzy-matching. Returns a word-object."
  (let* ((hrm-beta-input-type 'beta)
         (re (hrm--re-builder string))
         (entries (oref lexicon entries))
         (matches (hrm--re-matcher re (hash-table-keys entries))))
    (when matches
      (gethash (car matches) entries))))

(cl-defun hrm--display-word-buffer (word &optional (lexicon hrm-lsj))
  "Display WORD, a string or word-object, from LEXICON (default: LSJ)."
  (unless (hrm-word-p word)
    (if (stringp word)
        (setq word (hrm--string-to-object word lexicon))
      (error "Argument is neither a ‘hrm-word’ object nor a string: %s"
             word)))
  (when word
    (hrm--switch-buffer
     (hrm--word-buffer word))))

(provide 'hrm-completion)

;;; hrm-completion.el ends here
