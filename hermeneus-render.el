;;; hermeneus-render.el --- -*- lexical-binding: t -*-

;; [[id:TKR:aad6b242-9100-4505-9739-dbcfc187c036][Dependencies:1]]
(require 'gv)
(require 'widget)
(require 'tree-widget)
(require 'anaphora)
(require 'shr)
(require 'dom)
(require 'hermeneus-xml)
(require 'eieio)
(require 'seq)

(eval-when-compile (require 'cl-macs)
                   (require 'rx)
                   (require 'subr-x))

(require 'hermeneus-conv)
(require 'hermeneus-xml)
(require 'hermeneus-tags)

(defvar hermeneus--greek-punctuation)
;; Dependencies:1 ends here

;; [[id:TKR:6cfdaa3c-43a4-4acc-ac6f-ec4c0f33091c][Variables:1]]
(defcustom hermeneus-max-buffers 8
  "The maximum number of open Hermeneus word buffers.
If there are this many buffers open, then opening a new Hermeneus word
buffer will close the least recently visited one. Nil means no Hermeneus
buffers will be closed upon opening a new one.

(Note that quitting such a buffer with ‘quit-window’, ordinarily set to
the letter q, will send it to the back of the line; i.e., it will be
first to be closed when opening a new word buffer.)"
  :type '(choice (const nil) integer)
  :tag "Hermeneus — maximum number of buffers"
  :group 'hermeneus)

(defcustom hermeneus-show-entry-source nil
  "Whether to show the document source after a word definition.
This affects the word defintiions displayed by ‘describe-greek-word’ and
‘counsel-greek-word’.

When this is set to “XML” (symbol ‘xml’), then any definition displayed
will be followed by the definition’s original XML source. When this is
set to “DOM sexp” (symbol ‘sexp’), the source will be displayed in the
form of the DOM sexp that was generated from the XML by
‘libxml-parse-xml-region’ and used by Hermeneus to interpret and render
the definition. When this is set to “both” (symbol ‘both’, or t), then
the source will be displayed as XML and as a DOM sexp. When this is set
to “no” (nil), the default, then no source will be displayed below the
definition."
  :type '(choice (const xml :tag "XML")
                 (const sexp :tag "DOM sexp")
                 (const both :tag "both")
                 (const nil :tag "no"))
  :tag "Hermeneus — show entry source?"
  :group 'hermeneus)

(defcustom hermeneus-show-entry-source-tidy-p t
  "Whether to tidy the displayed XML source using Tidy.
Tidy is an external program that makes XML sources easier to
read. If it is not installed, or if “Hermeneus — show entry
source?” (‘hermeneus-show-entry-source-p’) is nil, this option is
ignored.

Find out more about Tidy at http://www.html-tidy.org"
  :type 'boolean
  :tag "Hermeneus — tidy entry source?"
  :group 'hermeneus)

(defcustom hermeneus-show-entry-source-tidy-config
  (expand-file-name "hermeneus-tidy.conf" (file-name-directory
                                     (locate-library "hermeneus")))
  "Configuration file to use with Tidy when tidying XML sources.
See options “Hermeneus — show entry source?”
(‘hermeneus-show-entry-source-p’) and “Hermeneus — tidy entry source?”
(‘hermeneus-show-entry-source-tidy’).

Find out more about Tidy at http://www.html-tidy.org"
  :type 'file
  :tag "Hermeneus — Tidy config file"
  :group 'hermeneus)
;; Variables:1 ends here

;; [[id:TKR:40231884-7d27-4fa7-a8a5-52bbab041a20][Functions:1]]
(defun hermeneus--roman-numeral-p (string)
  "A limited test for whether STRING represents a Roman numeral.
This is to allow proper spacing for Roman-numbered list bullets. The
limitation is that it can’t tell when a single letter (“I”, “V”, “X”,
etc.) is supposed to be a Roman numeral or not, and in those cases it
will return nil."
  (when (> (length string) 1)
    (string-match-p "\\`M?M?M?C?M?D?C?C?C?D?C?X?C?L?X?L?X?X?X?I?X?V?I?V?I?I?I?\\'" string)))

(defun hermeneus--margin-indent-width ()
  ;; The contents of this function used to be as follows:
  ;; (shr-string-pixel-width
  ;;  (propertize "XIII " :face 'hermeneus-default-face))
  ;; Sometime after I upgraded to Emacs 29, this started giving me an
  ;; error, saying that ‘shr-string-pixel-width’ is an invalid
  ;; function (it’s a macro). I’m not sure why this happens. But,
  ;; ‘shr-string-pixel-width’ is a pretty simple macro, so I’m
  ;; just reimplementing its functionality here.
  (let ((string (propertize "XIII " :face 'hermeneus-default-face)))
    (if (not shr-use-fonts)
        (length string)
      (string-pixel-width string))))

(defun hermeneus--pixel-column ()
  (let ((pos (point)))
    (prog1 (shr-pixel-column)
      (goto-char pos))))
;; Functions:1 ends here

;; [[id:TKR:8fcb947d-de04-41e0-9bd4-c9861511da2b][Widgets:1]]
(defvar hermeneus--tree-depth 0)
(defvar hermeneus--parent-tree nil)

(define-widget 'hermeneus-tree-widget 'tree-widget
  "A tree widget for displaying XML sources and DOM trees."
  :action 'hermeneus-tree-widget-action
  :expander 'hermeneus-tree-widget-expander)

(defun hermeneus--tree-icon-width (&optional icon-sym)
  (with-temp-buffer
    (widget-create (or icon-sym 'tree-widget-open-icon))
    (hermeneus--pixel-column)))

(defun hermeneus--get-dom-tree-widget (value &rest kwargs)
  (if (consp value)
      `(hermeneus-tree-widget :tag ,(when-let ((tag (dom-tag value)))
                                (propertize (symbol-name (dom-tag value))
                                            'face 'bold))
                        :hermeneus-value ,value
                        ,@kwargs)
    `(item :value ,(propertize (format "\"%s\"" value)
                               'hermeneus--tree hermeneus--parent-tree
                               'face 'font-lock-string-face))))

(defun hermeneus-tree-widget-expander (tree)
  (let* ((value (widget-get tree :hermeneus-value))
         (attrs-string (awhen (dom-attributes value)
                         (thread-first it
                           (pp-to-string)
                           (string-trim-right)
                           (propertize 'hermeneus--tree tree)))))
    (append (when attrs-string
              `((item :value ,attrs-string)))
            (awhen (dom-children value)
              (let ((hermeneus--parent-tree tree))
                (cl-loop for c in it
                         for i from (length it) downto 1
                         if (or (> i 1) (not (stringp c)))
                         collect (hermeneus--get-dom-tree-widget c)
                         else
                         collect (hermeneus--get-dom-tree-widget
                                  (propertize
                                   c 'hermeneus--tree-last-sibling t))))))))

(defun hermeneus-tree-widget-action (tree &optional event)
  (unwind-protect
      (progn
        (advice-add 'princ :before-until 'hermeneus--well-excuse-me-princ)
        (tree-widget-action tree event))
    (advice-remove 'princ 'hermeneus--well-excuse-me-princ)))

(defun hermeneus--tree-insert-guides (tree &optional icon-width last-sibling-p)
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
                                             (hermeneus--tree-icon-width))))))))

(defun hermeneus--well-excuse-me-princ (object &optional printcharfun)
  "Function with which to override ‘princ’ to preserve text properties.
Well excu-u-u-u-use me, ‘princ’!"
  ;; This function is meant to be used as ‘before-until’ advice.
  (when (and (stringp object) (bufferp printcharfun))
    (when-let ((tree (get-text-property 0 'hermeneus--tree object))
               (wrap (with-temp-buffer
                       (hermeneus--tree-insert-guides tree)
                       (hermeneus--pixel-column))))
      (cl-callf propertize object 'wrap-prefix `(space :width (,wrap)))
      (if-let ((first-line-end (string-match-p "\n" object))
               (first-line     (substring object 0 (1+ first-line-end)))
               (rest-lines     (substring object (1+ first-line-end)))
               (icon-width     (hermeneus--tree-icon-width)))
          (let ((last-sibling-p (get-text-property 1 'hermeneus--tree-last-sibling object)))
            (with-current-buffer printcharfun
              (insert first-line)
              (dolist (l (split-string rest-lines "\n"))
                (hermeneus--tree-insert-guides tree icon-width last-sibling-p)
                (insert l ?\n))
              (delete-char -1)))
        (insert object))
      object)))
;; Widgets:1 ends here

;; [[id:TKR:fc1834d0-b144-4892-88ba-59ce73dd836d][Help buffers:1]]
(defun hermeneus--word-buffer (obj)
  "Get or create a Hermeneus buffer for word-object OBJ and return it."
  (with-current-buffer (get-buffer-create (format "*Hermeneus: %s *" (oref obj key)))
    (hermeneus-mode)
    (setq hermeneus--word-obj obj
          hermeneus--word-dom (hermeneus--get-dom-from-word obj))
    (hermeneus--dom-convert-betacode hermeneus--word-dom)
    (hermeneus-buffer-update)
    (current-buffer)))

(defun hermeneus--dom-convert-betacode (dom)
  "Destructively convert all Greek Betacode text in DOM to Unicode."
  (cl-loop for elt in-ref (dom-elements dom 'lang "greek")
           do (cl-loop for child in-ref (dom-children elt)
                       if (and (stringp child)
                               (not (string-match-p
                                     (rx (category greek)) child)))
                       do (cl-callf hermeneus-beta-to-unicode child))))

(defun hermeneus--get-rendering-functions-alist ()
  (cl-loop for tag in hermeneus-defined-tags
           collect (cons tag (intern-soft (format "hermeneus-render-%s" tag)))))

(defun hermeneus-buffer-update ()
  (interactive)
  (let ((shr-external-rendering-functions
         (hermeneus--get-rendering-functions-alist))
        (inhibit-read-only t)
        (pos (point)))
    (with-silent-modifications
      (erase-buffer)
      (shr-insert-document hermeneus--word-dom)
      (when hermeneus-show-entry-source
        (insert "\n══════════════════════╡ Source ╞══════════════════════\n\n")
        (when (memq hermeneus-show-entry-source '(xml both t))
          (let ((widget `(hermeneus-tree-widget :tag "XML source")))
            (widget-put widget :args `((item :value ,(propertize
                                                      (hermeneus--get-entry-source hermeneus--word-obj)
                                                      'hermeneus--tree widget
                                                      'hermeneus--tree-last-sibling t))))
            (widget-create widget)))
        (when (memq hermeneus-show-entry-source '(sexp both t))
          (widget-create
           `(hermeneus-tree-widget :tag "DOM tree"
                             :hermeneus-value ,hermeneus--word-dom))))
      (goto-char pos))))
;; Help buffers:1 ends here

;; [[id:TKR:2c8fd835-90e1-4e54-a923-2da89407f579][Switch buffer:1]]
(defun hermeneus--switch-buffer (buffer)
  ;; Is the current window a Hermeneus buffer? Switch in the same window.
  (if (eq major-mode 'hermeneus-mode)
      (pop-to-buffer-same-window buffer)
    ;; Is any window on the current frame a Hermeneus buffer? Switch
    ;; in that window.
    (if-let ((window (cl-loop for x in (window-list)
                              if (eq (with-selected-window x major-mode)
                                     'hermeneus-mode)
                              return x)))
        (progn (select-window window) (pop-to-buffer-same-window buffer))
      ;; Otherwise, use ‘pop-to-buffer’.
      (pop-to-buffer buffer))))
;; Switch buffer:1 ends here

;; [[id:TKR:2e7d2ea0-72e2-4903-80ac-2ac3f2980f71][Get entry source:1]]
(cl-defun hermeneus--get-entry-source (&optional (obj hermeneus--word-obj))
  (seq-let (i begin end) (oref obj loc)
    (let ((bufstr (hermeneus--get-dom-from-file i begin end :plain-xml-p t)))
      (with-temp-buffer
        (insert bufstr)
        (hermeneus-tidy-xml-buffer)
        (hermeneus-indent-xml-buffer)
        ;; fontify the source—thanks go to Wilfred Hughes’s ‘helpful’
        ;; package for a good example of how to do this (in function
        ;; ‘helpful--syntax-highlight’)
        (delay-mode-hooks
          (if (fboundp 'xml-mode)
              (nxml-mode)
            (xml-mode)))
        (if (fboundp 'font-lock-ensure)
            (font-lock-ensure)
          (with-no-warnings
            (font-lock-fontify-buffer)))
        (buffer-string)))))

;; (cl-defun hermeneus--get-entry-sexp (&optional (obj hermeneus--word-obj))
;;   (seq-let (i begin end) (oref obj loc)
;;     (let ((bufstr (hermeneus--get-dom-from-file i begin end)))
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
;; Get entry source:1 ends here

;; [[id:TKR:4d3df8d9-777d-4421-ad3a-a10912a34890][Tidying:1]]
(defun hermeneus-tidy-xml-buffer ()
  (interactive)
  (if (and hermeneus-show-entry-source-tidy-p (executable-find "tidy"))
      (call-process-region 1 (point-max) "tidy" t t nil
                           "-config" hermeneus-show-entry-source-tidy-config "-")))

(defun hermeneus-indent-xml-buffer ()
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
;; Tidying:1 ends here

;; [[id:TKR:1b45d816-5cc7-441a-b9cb-70863a09ba98][Clear out old Hermeneus buffers:1]]
(defun hermeneus--clear-old-buffers ()
  "Clear out old Hermeneus buffers to satisfy ‘hermeneus-max-buffers’."
  ;; with acknowledgement to Wilfred Hughes’s ‘helpful’ package
  ;; and the function ‘helpful--buffer’
  (when (numberp hermeneus-max-buffers)
    (let* ((hermeneus-buffers
            (seq-filter (lambda (x) (with-current-buffer x
                                      (eq major-mode 'hermeneus-mode)))
                        (buffer-list)))
           ;; The result of the function ‘buffer-list’ is ordered by
           ;; most recently visited first; so, define everything past
           ;; the ‘hermeneus-max-buffers’ limit as excess
           (excess-buffers (nthcdr hermeneus-max-buffers hermeneus-buffers)))
      (mapc #'kill-buffer excess-buffers))))
;; Clear out old Hermeneus buffers:1 ends here

(provide 'hermeneus-render)

;;; hermeneus-render.el ends here
