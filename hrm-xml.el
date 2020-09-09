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

(require 'hrm-conv)

(defun hrm--url-p (path)
  "Return non-nil if PATH is a valid URL.
Specifically, this will return a parsed URL object from
‘url-generic-parse-url’, otherwise nil."
  (let ((url (url-generic-parse-url path)))
    (when (cl-struct-slot-value 'url 'type url)
      url)))

(cl-defun hrm--get-next-tag (&optional (tag "entryFree"))
  "Return start and end positions of the next instance of XML tag TAG
 (defaults to “entryFree”). Move point to the end position."
  (save-match-data
    (when (search-forward (concat "<" tag) nil t)
      (let* ((begin (goto-char (match-beginning 0)))
             (end (progn (search-forward (concat "</" tag ">") nil t)
                         (point))))
        (list begin end)))))

(cl-defun hrm--get-dom-from-file (file &optional start end
                                       &key plain-xml-p)
  "Return a DOM sexp from the XML file FILE.
If keyword argument PLAIN-XML-P is non-nil, return plain XML instead."
  (when (integerp file)
    (setq file (nth file hrm-lsj-files)))
  (with-temp-buffer
    (hrm--insert-contents file)
    (funcall (if plain-xml-p
                 'buffer-substring
               'libxml-parse-xml-region)
             (or start (point-min))
             (or end (point-max)))))

(defun hrm--insert-contents (file)
  "Insert contents of FILE into the current buffer.
FILE can be a local filename or an URL."
  (if-let ((url (hrm--url-p file)))
      (let ((buffer (url-retrieve-synchronously url nil t 60)))
        (url-insert-buffer-contents buffer url)
        (kill-buffer buffer))
    (if (file-exists-p file)
        (insert-file-contents file)
      (error "File does not exist: %s" file))))

(defun hrm--get-file-sizes (list)
  (let ((sizes '(42923474  5014862  4182729 14588543 40082401
                    15614  1233434  2872155  4731605  4600309
                 23622167  6753069 12285441  4142048   922716
                 12279541 38221861   676533   670125  2249926
                 22838928 11626884  9107698  8185312  6534345
                  1596622  1656586)))
    (cl-loop for l in list
             for i from 1 to (length list)
             if (hrm--url-p l)
             collect (nth (1- i) sizes)
             else
             collect (nnheader-file-size l))))

(defun hrm-scan-lsj ()
  (interactive)
  (oset hrm-lsj entries (hrm-scan-entries))
  (eieio-persistent-save hrm-lsj))

(defun hrm-scan-entries ()
  "Scan over every lexicon entry in the LSJ, using ‘hrm-scan-entry’.
Return a hash table."
  (interactive)
  (let* ((hash (make-hash-table :test 'equal :size 116493))
         (sizes (hrm--get-file-sizes hrm-lsj-files))
         (total 0)
         (prog-msg "Scanning Liddell and Scott")
         (progress (make-progress-reporter prog-msg
                                           0 (apply '+ sizes))))
    (dotimes (i (length hrm-lsj-files))
      (with-temp-buffer
        (hrm--insert-contents (nth i hrm-lsj-files))
        (let ((cur-size (pop sizes))
              (max (point-max)))
          (awhile (hrm--get-next-tag "entryFree")
            (oset (hrm-scan-entry (apply 'libxml-parse-xml-region it) hash) loc (cons i it))
            (progress-reporter-update progress (+ total
                                                  (* cur-size
                                                     (/ (float (cadr it))
                                                        max)))))
          (progress-reporter-update progress (cl-incf total cur-size)))))
    (progress-reporter-done progress)
    hash))

(defun hrm-scan-entry (entry &optional HASH)
  "Scan ENTRY, a DOM sexp of an “entryFree” tag from the LSJ files.
Identify its headword and numeric ID. Create a word object. Add
the headword and object as a key-value pair in hash-table HASH,
if present. Run each function from ‘hrm-scan-entry-functions’
with two arguments, the word object and ENTRY. Finally, return
the object."
  (let* ((key (hrm-beta-to-unicode (dom-attr entry 'key)))
         (id (string-to-number
              (string-remove-prefix "n" (dom-attr entry 'id))))
         (obj (hrm-word :key key :id id)))
    (when hash (puthash key obj hash))
    (run-hook-with-args 'hrm-scan-entry-functions obj entry)
    obj))



(provide 'hrm-xml)

;; hrm-xml.el ends here
