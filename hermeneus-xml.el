;;; hermeneus-xml.el --- -*- lexical-binding: t -*-

(require 'url-handlers)
(require 'url)
(require 'url-parse)
(require 'eieio-base)
(require 'cl-lib)
(require 'eieio)
(require 'anaphora)
(require 'dom)
(require 'nnheader)

(eval-when-compile (require 'subr-x)
                   (require 'cl-macs))

(require 'hermeneus-conv)

(defvar hermeneus--greek-punctuation)

(defun hermeneus--url-p (path)
  "Return non-nil if PATH is a valid URL.
Specifically, this will return a parsed URL object from
  ‘url-generic-parse-url’, otherwise nil."
  (let ((url (url-generic-parse-url path)))
    (when (cl-struct-slot-value 'url 'type url)
      url)))

(cl-defun hermeneus--get-next-tag (&optional (tag "entryFree"))
  "Return start and end positions of the next instance of XML tag TAG
(defaults to “entryFree”). Move point to the end position."
  (save-match-data
    (when (re-search-forward (concat "<" (regexp-quote tag)
                                     (rx word-end))
                             nil t)
      (let* ((begin (goto-char (match-beginning 0)))
             (end (progn (search-forward (concat "</" tag ">") nil t)
                         (point))))
        (list begin end)))))

(cl-defun hermeneus--get-dom-from-file (file &optional start end
                                       &key plain-xml-p)
  "Return a DOM sexp from the XML file FILE.
If keyword argument PLAIN-XML-P is non-nil, return plain XML instead."
  (when (integerp file)
    (setq file (nth file hermeneus-lsj-files)))
  (with-temp-buffer
    (hermeneus--insert-contents file)
    (funcall (if plain-xml-p
                 'buffer-substring
               'libxml-parse-xml-region)
             (or start (point-min))
             (or end (point-max)))))

(defun hermeneus--insert-contents (file)
  "Insert contents of FILE into the current buffer.
FILE can be a local filename or an URL."
  (if-let ((url (hermeneus--url-p file)))
      (let ((buffer (url-retrieve-synchronously url nil t 60)))
        (url-insert-buffer-contents buffer url)
        (kill-buffer buffer))
    (if (file-exists-p file)
        (insert-file-contents file)
      (error "File does not exist: %s" file))))

(defun hermeneus--get-dom-from-word (word)
  "Return the DOM from the XML LSJ definition of word-object WORD."
  (apply #'hermeneus--get-dom-from-file (oref word loc)))

(cl-defun hermeneus--get-lsj-file-sizes (&optional (list hermeneus-lsj-files))
  "Return the sizes of the XML LSJ files in LIST.
LIST defaults to the value of ‘hermeneus-lsj-files’, and is assumed to
be a list of the files in the XML LSJ. If any filename in LIST is an
URL, then the size is given from a prerecorded list. Otherwise,
‘nnheader-file-size’ is used to find the file’s size."
  (let ((sizes '(42923474  5014862  4182729 14588543 40082401
                             15614  1233434  2872155  4731605  4600309
                          23622167  6753069 12285441  4142048   922716
                          12279541 38221861   676533   670125  2249926
                          22838928 11626884  9107698  8185312  6534345
                           1596622  1656586)))
    (cl-loop for l in list
             for i from 1 to (length list)
             if (hermeneus--url-p l)
             collect (nth (1- i) sizes)
             else
             collect (nnheader-file-size l))))

(defvar hermeneus-lsj-files nil)

(cl-defun hermeneus--set-lsj-dir (&optional (symbol 'hermeneus-lsj-dir)
                                      (value (if (boundp 'hermeneus-lsj-dir)
                                                 hermeneus-lsj-dir
                                               hermeneus--git-lsj-dir)))
  "Setter function for ‘hermeneus-lsj-dir’."
  (set-default symbol value)
  (setq hermeneus-lsj-files
        (cl-loop for i from 1 to 27
                 with expand-func = (if (hermeneus--url-p value)
                                        'url-expand-file-name
                                      'expand-file-name)
                 collect (funcall expand-func
                                  (format "grc.lsj.perseus-eng%s.xml" i)
                                  value))))

(defcustom hermeneus-lsj-dir hermeneus--git-lsj-dir
  "Directory where the LSJ Greek lexicon files can be found.
This can be an URL or a local file path. The files themselves should
be named in the format “grc.lsj.perseus-engXX.xml”, where XX is a
number from 1 to 27 (no padding).

If you set this outside of Customize, be sure to evaluate
‘hermeneus--set-lsj-dir’."
  :tag "Hermeneus — LSJ directory"
  :type `(choice (const ,hermeneus--git-lsj-dir
                        :tag "Perseus Digital Library’s Git repository")
                 (directory :tag "local directory")
                 (string :tag "URL"))
  :set 'hermeneus--set-lsj-dir
  :group 'hermeneus)

(defvar hermeneus-use-fonts t)

(defun hermeneus-scan-entries ()
  "Scan over every lexicon entry in the LSJ, using ‘hermeneus-scan-entry’.
Return a hash table mapping each headword (expressed as a string) to its
corresponding word object."
  (interactive)
  (let* ((hash (make-hash-table :test 'equal :size 116493))
         (sizes (hermeneus--get-lsj-file-sizes))
         (total 0)
         (prog-msg "Scanning Liddell and Scott")
         (progress (make-progress-reporter prog-msg
                                           0 (apply '+ sizes))))
    (dotimes (i (length hermeneus-lsj-files))
      (with-temp-buffer
        (hermeneus--insert-contents (nth i hermeneus-lsj-files))
        (let ((cur-size (pop sizes))
              (max (point-max)))
          (awhile (hermeneus--get-next-tag "entryFree")
            (oset (hermeneus-scan-entry (apply 'libxml-parse-xml-region it) hash) loc (cons i it))
            (progress-reporter-update progress (+ total
                                                  (* cur-size
                                                     (/ (float (cadr it))
                                                        max)))))
          (progress-reporter-update progress (cl-incf total cur-size)))))
    (progress-reporter-done progress)
    hash))

(defun hermeneus-scan-entry (entry &optional hash)
  "Scan ENTRY, a DOM sexp of an “entryFree” tag from the LSJ files.
Identify its headword and numeric ID. Create a word object. Add the
headword and object as a key-value pair in hash-table HASH, if present.
Run each function from ‘hermeneus-scan-entry-functions’ with two
arguments, the word object and ENTRY. Finally, return the object."
  (let* ((key (hermeneus-beta-to-unicode (dom-attr entry 'key)))
         (id (string-to-number
              (string-remove-prefix "n" (dom-attr entry 'id))))
         (obj (hermeneus-word :key key :id id)))
    (when hash (puthash key obj hash))
    (run-hook-with-args 'hermeneus-scan-entry-functions obj entry)
    obj))



(defun hermeneus-get-entries (lexicon)
  "Access ‘entries’ slot of ‘hermeneus-lexicon’ object LEXICON.
The ‘entries’ slot of ‘hermeneus-lexicon’ objects is a hash table
containing ‘hermeneus-word’ objects. The reason to use this function
instead of using (oref LEXICON entries) is because a newly created
‘hermeneus-lexicon’ object will not have any ‘hermeneus-word’ objects in
its ‘entries’ slot, and will need to be populated."
  (unless (hermeneus-lexicon-p lexicon)
    (signal 'wrong-type-argument (list 'hermeneus-lexicon-p lexicon)))
  (unless (oref lexicon initialized-p)
    (hermeneus--populate-lexicon lexicon))
  (oref lexicon entries))

(defun hermeneus--populate-lexicon (lexicon)
  "Populate the ‘entries’ hash-table of ‘hermeneus-lexicon’ object LEXICON
with word-objects from the LSJ."
  (oset lexicon entries (hermeneus-scan-entries))
  (oset lexicon initialized-p t)
  (eieio-persistent-save lexicon))

;;;###autoload
(defun hermeneus-scan-lsj ()
  "Scan the LSJ and save the resulting word-objects to ‘hermeneus-lsj’."
  (interactive)
  (hermeneus--populate-lexicon hermeneus-lsj))

(provide 'hermeneus-xml)

;;; hermeneus-xml.el ends here
