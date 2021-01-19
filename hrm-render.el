(require 'gv)
(require 'widget)
(require 'tree-widget)
(require 'anaphora)
(require 'shr)
(require 'dom)
(require 'hrm-xml)
(require 'eieio)
(require 'seq)

(eval-when-compile (require 'cl-macs)
                   (require 'rx)
                   (require 'subr-x))

(require 'hrm-conv)
(require 'hrm-xml)

(defcustom hrm-show-entry-source nil
  "Whether to show the document source after a word definition.
This affects the word defintiions displayed by ‘describe-greek-word’
and ‘counsel-greek-word’.

When this is set to “XML” (symbol ‘xml’), then any definition
displayed will be followed by the definition’s original XML
source. When this is set to “DOM sexp” (symbol ‘sexp’), the
source will be displayed in the form of the DOM sexp that was
generated from the XML by ‘libxml-parse-xml-region’ and used by
Hermeneus to interpret and render the definition. When this is
set to “both” (symbol ‘both’, or t), then the source will be
displayed as XML and as a DOM sexp. When this is set to
“no” (nil), the default, then no source will be displayed below
the definition."
  :type '(choice (const xml :tag "XML")
                 (const sexp :tag "DOM sexp")
                 (const both :tag "both")
                 (const nil :tag "no"))
  :tag "Hermeneus — show entry source?"
  :group 'hermeneus)

(defcustom hrm-show-entry-source-tidy-p t
  "Whether to tidy the displayed XML source using Tidy.
Tidy is an external program that makes XML sources easier to
read. If it is not installed, or if “Hermeneus — show entry
source?” (‘hrm-show-entry-source-p’) is nil, this option is
ignored.

Find out more about Tidy at http://www.html-tidy.org"
  :type 'boolean
  :tag "Hermeneus — tidy entry source?"
  :group 'hermeneus)

(defcustom hrm-show-entry-source-tidy-config
  (expand-file-name "hrm-tidy.conf" (file-name-directory
                                     (locate-library "hermeneus")))
  "Configuration file to use with Tidy when tidying XML sources.
See options “Hermeneus — show entry source?” (‘hrm-show-entry-source-p’)
and “Hermeneus — tidy entry source?” (‘hrm-show-entry-source-tidy’).

Find out more about Tidy at http://www.html-tidy.org"
  :type 'file
  :tag "Hermeneus — Tidy config file"
  :group 'hermeneus)

(defun hrm--roman-numeral-p (string)
  "A limited test for whether STRING represents a Roman numeral.
This is to allow proper spacing for Roman-numbered list bullets.
The limitation is that it can’t tell when a single letter (“I”, “V”,
“X”, etc.) is supposed to be a Roman numeral or not, and in those
cases it will return nil."
  (when (> (length string) 1)
    (string-match-p "\\`M?M?M?C?M?D?C?C?C?D?C?X?C?L?X?L?X?X?X?I?X?V?I?V?I?I?I?\\'" string)))

(defun hrm--margin-indent-width ()
  (shr-string-pixel-width
   (propertize "XIII " :face 'hrm-default-face)))

(defun hrm--pixel-column ()
  (let ((pos (point)))
    (prog1 (shr-pixel-column)
      (goto-char pos))))

(defvar hrm--tree-depth 0)
(defvar hrm--parent-tree nil)

(define-widget 'hrm-tree-widget 'tree-widget
  "A tree widget for displaying XML sources and DOM trees."
  :action 'hrm-tree-widget-action
  :expander 'hrm-tree-widget-expander)

(defun hrm--tree-icon-width (&optional icon-sym)
  (with-temp-buffer
    (widget-create (or icon-sym 'tree-widget-open-icon))
    (hrm--pixel-column)))

(defun hrm--get-dom-tree-widget (value &rest kwargs)
  (if (consp value)
      `(hrm-tree-widget :tag ,(when-let ((tag (dom-tag value)))
                                (propertize (symbol-name (dom-tag value))
                                            'face 'bold))
                        :hrm-value ,value
                        ,@kwargs)
    `(item :value ,(propertize (format "\"%s\"" value)
                               'hrm--tree hrm--parent-tree
                               'face 'font-lock-string-face))))

(defun hrm-tree-widget-expander (tree)
  (let* ((value (widget-get tree :hrm-value))
         (attrs-string (awhen (dom-attributes value)
                         (thread-first it
                           (pp-to-string)
                           (string-trim-right)
                           (propertize 'hrm--tree tree)))))
    (append (when attrs-string
              `((item :value ,attrs-string)))
            (awhen (dom-children value)
              (let ((hrm--parent-tree tree))
                (cl-loop for c in it
                         for i from (length it) downto 1
                         if (or (> i 1) (not (stringp c)))
                         collect (hrm--get-dom-tree-widget c)
                         else
                         collect (hrm--get-dom-tree-widget
                                  (propertize
                                   c 'hrm--tree-last-sibling t))))))))

(defun hrm-tree-widget-action (tree &optional event)
  (unwind-protect
      (progn
        (advice-add 'princ :before-until 'hrm--well-excuse-me-princ)
        (tree-widget-action tree event))
    (advice-remove 'princ 'hrm--well-excuse-me-princ)))

(defun hrm--tree-insert-guides (tree &optional icon-width last-sibling-p)
  (let ((flags    (aif (widget-get tree :tree-widget--guide-flags)
                      (append (list t) it)
                    (if last-sibling-p
                        (list nil)
                      (list t))))
        (guide    (widget-get tree :guide))
        (noguide  (widget-get tree :no-guide))
        (guidi    (tree-widget-find-image "guide"))
        (noguidi  (tree-widget-find-image "no-guide"))
        (nohandle (widget-get tree :no-handle))
        (nohandli (tree-widget-find-image "no-handle")))
    ;; From ‘tree-widget.el’.
    (dolist (f (reverse flags))
      (widget-create-child-and-convert
       tree (if f guide noguide)
       :tag-glyph (if f guidi noguidi))
      (widget-create-child-and-convert
       tree nohandle :tag-glyph nohandli))
    (insert (propertize " " 'display
                        `(space :width (,(or icon-width
                                             (hrm--tree-icon-width))))))))

(defun hrm--well-excuse-me-princ (object &optional printcharfun)
  "Function with which to override ‘princ’ to preserve text properties.
Well excu-u-u-u-use me, ‘princ’!"
  ;; This function is meant to be used as ‘before-until’ advice.
  (when (and (stringp object) (bufferp printcharfun))
    (when-let ((tree (get-text-property 0 'hrm--tree object))
               (wrap (with-temp-buffer
                       (hrm--tree-insert-guides tree)
                       (hrm--pixel-column))))
      (cl-callf propertize object 'wrap-prefix `(space :width (,wrap)))
      (if-let ((first-line-end (string-match-p "\n" object))
               (first-line     (substring object 0 (1+ first-line-end)))
               (rest-lines     (substring object (1+ first-line-end)))
               (icon-width     (hrm--tree-icon-width)))
          (let ((last-sibling-p (get-text-property 1 'hrm--tree-last-sibling object)))
            (with-current-buffer printcharfun
              (insert first-line)
              (dolist (l (split-string rest-lines "\n"))
                (hrm--tree-insert-guides tree icon-width last-sibling-p)
                (insert l ?\n))
              (delete-char -1)))
        (insert object))
      object)))

(defun hrm--word-buffer (obj)
  (with-current-buffer (get-buffer-create (format "*Hermeneus: %s *" (oref obj key)))
    (hermeneus-mode)
    (setq hrm--word-obj obj
          hrm--word-dom (apply 'hrm--get-dom-from-file (oref obj loc)))
    (hrm--dom-convert-betacode hrm--word-dom)
    (hrm-buffer-update)
    (current-buffer)))

(defun hrm--dom-convert-betacode (dom)
  "Destructively convert all Greek Betacode text in DOM to Unicode."
  (cl-loop for elt in-ref (dom-elements dom 'lang "greek")
           do (cl-loop for child in-ref (dom-children elt)
                       if (and (stringp child)
                               (not (string-match-p
                                     (rx (category greek)) child)))
                       do (cl-callf hrm-beta-to-unicode child))))

(defun hrm--get-rendering-functions-alist ()
  (cl-loop for tag in hrm-defined-tags
           collect (cons tag (intern-soft (format "hrm-render-%s" tag)))))

(defun hrm-buffer-update ()
  (interactive)
  (let ((shr-external-rendering-functions
         (hrm--get-rendering-functions-alist))
        (inhibit-read-only t)
        (pos (point)))
    (with-silent-modifications
      (erase-buffer)
      (shr-insert-document hrm--word-dom)
      (when hrm-show-entry-source
        (insert "\n══════════════════════╡ Source ╞══════════════════════\n\n")
        (when (memq hrm-show-entry-source '(xml both t))
          (let ((widget `(hrm-tree-widget :tag "XML source")))
            (widget-put widget :args `((item :value ,(propertize
                                                      (hrm--get-entry-source hrm--word-obj)
                                                      'hrm--tree widget
                                                      'hrm--tree-last-sibling t))))
            (widget-create widget)))
        (when (memq hrm-show-entry-source '(sexp both t))
          (widget-create
           `(hrm-tree-widget :tag "DOM tree"
                             :hrm-value ,hrm--word-dom))))
      (goto-char pos))))

(defun hrm--switch-buffer (buffer)
  ;; Is the current window a Hermeneus buffer? Switch in the same window.
  (if (eq major-mode 'hermeneus-mode)
      (pop-to-buffer-same-window buffer)
    ;; Is any window on the current frame a Hermeneus buffer? Switch in that window.
    (if-let ((window (cl-loop for x in (window-list)
                              if (eq (with-selected-window x major-mode) 'hermeneus-mode)
                              return x)))
        (progn (select-window window) (pop-to-buffer-same-window buffer))
      ;; Otherwise, use ‘pop-to-buffer’.
      (pop-to-buffer buffer))))

(cl-defun hrm--get-entry-source (&optional (obj hrm--word-obj))
  (seq-let (i begin end) (oref obj loc)
    (let ((bufstr (hrm--get-dom-from-file i begin end :plain-xml-p t)))
      (with-temp-buffer
        (insert bufstr)
        (hrm-tidy-xml-buffer)
        (hrm-indent-xml-buffer)
        ;; fontify the source—thanks go to Wilfred Hughes’s
        ;; ‘helpful’ package for a good example of how to do this
        ;; (in function ‘helpful--syntax-highlight’)
        (delay-mode-hooks
          (if (fboundp 'xml-mode)
              (nxml-mode)
            (xml-mode)))
        (if (fboundp 'font-lock-ensure)
            (font-lock-ensure)
          (with-no-warnings
            (font-lock-fontify-buffer)))
        (buffer-string)))))

;; (cl-defun hrm--get-entry-sexp (&optional (obj hrm--word-obj))
;;   (seq-let (i begin end) (oref obj loc)
;;     (let ((bufstr (hrm--get-dom-from-file i begin end)))
;;       (with-temp-buffer
;;         (pp bufstr (current-buffer))
;;         (delay-mode-hooks (emacs-lisp-mode))
;;         (when (fboundp 'rainbow-delimiters-mode)
;;           (rainbow-delimiters-mode))
;;         (if (fboundp 'font-lock-ensure)
;;             (font-lock-ensure)
;;           (with-no-warnings
;;             (font-lock-fontify-buffer)))
;;         (buffer-string)))))

(defun hrm-tidy-xml-buffer ()
  (interactive)
  (if (and hrm-show-entry-source-tidy-p (executable-find "tidy"))
      (call-process-region 1 (point-max) "tidy" t t nil
                           "-config" hrm-show-entry-source-tidy-config "-")))

(defun hrm-indent-xml-buffer ()
  (interactive)
  ;; Tidy and ‘nxml-mode’ both royally fail at
  ;; indenting the LSJ’s XML properly, so we’ll
  ;; press ‘sgml-mode’ into service.
  (when (fboundp 'sgml-mode)
    (let ((mode major-mode))
      (delay-mode-hooks (sgml-mode))
      (goto-char (point-min))
      (while (< (point) (point-max))
        (sgml-indent-line)
        (forward-line))
      (funcall mode))))



(provide 'hrm-render)

;; hrm-render.el ends here
