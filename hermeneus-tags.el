;;; hermeneus-tags.el --- -*- lexical-binding: t -*-

(require 'gv)
(require 'seq)
(require 'shr)
(require 'dom)

(eval-when-compile (require 'cl-macs)
                   (require 'subr-x))

(require 'hermeneus-xml)
(require 'hermeneus-conv)
(require 'hermeneus-render)
(require 'hermeneus-abbr) ; standalone file

(defvar hermeneus--greek-punctuation)

(defface hermeneus-default-face '((t nil))
  "Default face for Hermeneus text display."
  :tag "Hermeneus — default face"
  :group 'hermeneus-faces)

(defvar hermeneus-tei-tags '(foreign cit name abbr date pb bibl biblScope
                               title author entryFree sense gramGrp etym
                               orth pron gen number per tns mood itype pos
                               subc tr))

(defvar hermeneus--prev-tag nil)
(defvar hermeneus--prev-author nil)

(defvar hermeneus--bibl-urn nil
  "The CTS URN cited by the current bibl tag.
Should be nil if Hermeneus is not currently rendering a bibl tag.")

(defvar hermeneus-defined-tags nil)

(defvar hermeneus--tag-keywords '(:attrs :face :render :doc-source))

(defvar-local hermeneus--word-obj nil)
(defvar-local hermeneus--word-dom nil)

(defvar-local hermeneus-doc-source nil)

(defcustom hermeneus-expand-abbreviations t
  "Whether abbreviations in definitions should be expanded.
If non-nil, Hermeneus will attempt to expand abbreviations (of authors,
works, etc.) found when displaying a definition. The abbreviation will
be available as the ‘help-echo’ (“tooltip”) property of the expanded
text. If nil, abbreviations will remain, with expansions written as
‘help-echo’ properties."
  :type 'boolean
  :tag "Hermeneus — expand abbreviations"
  :group 'hermeneus)

(defun hermeneus--render-generic (dom &optional tag face)
  (unless tag
    (setq tag (dom-tag dom)))
  (unless face
    (setq face (intern-soft (format "hermeneus-face-%s" tag))))
  (hermeneus--insert-space-maybe)
  (shr-fontize-dom dom face)
  (setq hermeneus--prev-tag tag))

(defun hermeneus--insert-space-maybe ()
  (unless (or (bobp) (bolp)
              (let ((cs (char-syntax (char-before))))
                (or (eq cs 32) (eq cs ?\())))
    (shr-insert " ")))

(defun hermeneus-greek-word-button-action (button)
  (hermeneus--display-word-buffer (button-get button 'target)))

(define-button-type 'hermeneus-greek-word-button
  'action #'hermeneus-greek-word-button-action
  'target nil)

;; The reason for this function, and its use in Hermeneus macros, is
;; to make it easier for non-Lisp people to make customizations.
;; (i.e., learning Ancient Greek is hard enough without having to
;; remember how Lisp plists work)
(defun hermeneus--normalize-keywords (list)
  "Make a proper plist from a list of keyword arguments.
For example, “'(:hero sonic tails knuckles :villain eggman)”
will return “'(:hero (sonic tails knuckles) :villain eggman)”,
which is more readable to ‘plist-get’ and related functions."
  (let (rtn)
    (while list
      (if (or (keywordp (car list))
              (keywordp (cadr list))
              (null (cadr list)))
          (push (pop list) rtn)
        (push (cl-loop repeat (length list)
                       until (keywordp (car list))
                       collect (pop list))
              rtn)))
    (nreverse rtn)))

(defun hermeneus--doc-concat (&rest strings)
  (with-temp-buffer
    (insert (apply 'concat strings))
    (goto-char 1)
    (while (not (eq (line-end-position) (point-max)))
      (forward-line)
      (unless (eq (line-beginning-position) (line-end-position))
        (fill-region (line-beginning-position) (line-end-position))))
    (buffer-string)))

(defun hermeneus--doc-source (doc-source)
  (when (and (symbolp doc-source) (boundp doc-source))
    (setq doc-source (symbol-value doc-source)))
  (cond ((stringp doc-source)
         (format "\n\nInformation from %s" doc-source))
        ((and (listp doc-source)
              (stringp (car doc-source)) (stringp (cadr doc-source)))
         (format "\n\nInformation from “%s”\n%s" (cadr doc-source) (car doc-source)))))

(defun hermeneus--format-attrs (attrs)
  (cl-loop while attrs
           concat (let* ((attr (pop attrs))
                         (desc (pop attrs)))
                    (format "\n‘%s’ %s" attr desc))))

(cl-defmacro define-hermeneus-tag (tag &rest args)
  "Macro for defining XML tags in Hermeneus.
TAG is the unquoted name of the tag in question.

DOCSTRING is an optional description that, when given, will be prepended
with the name of the tag and used in docstrings for constructs defined
by ‘define-hermeneus-tag’. e.g. the docstring used in the
‘define-hermeneus-tag’ definition for <name> is \"contains a proper noun
or noun phrase.\", and the docstring generated for the function
‘hermeneus-render-name’ includes \"‘<name>’ contains a proper noun or
noun phrase.\" Do not use line breaks to wrap the string; line breaks
will be added automatically to the generated docstrings.

‘define-hermeneus-tag’ accepts the following keyword arguments:

ATTRS is a list (though it can be expressed inline; see the function
‘hermeneus---normalize-keywords’) of tag attributes, in the form
\"ATTR DOCSTRING ATTR DOCSTRING …\". Each ATTR is the unquoted name of
an attribute specific to this tag, and each DOCSTRING is a short string
describing the preceding attribute. Like with the main docstring for the
tag, do not use line breaks for wrapping, and expect each string to be
prepended with the name of the attribute.

FACE is a quoted face specification which Hermeneus will use when
displaying the tag. See Info node ‘(elisp)Defining Faces’. If FACE is
not given, then a generic face will be created which by default inherits
from ‘hermeneus-default-face’. Either way, the resulting face will be
named in the format ‘hermeneus-face-TAG’.

RENDER is a series of sexps which will be used to define a function for
rendering the tag’s contents. The function will be named in the format
`hermeneus-render-TAG’ and will be given one argument, ‘dom’, which is
the DOM of the tag being rendered (see Info node ‘(elisp)Document Object
Model’). If RENDER is absent, then the special function
‘hermeneus--render-generic’ will be used to render the tag. Note that if
you define your own rendering function using RENDER, then any rendering
it does must manually take into account the tag’s face, which (as you
may recall) is named in the format ‘hermeneus-face-TAG’.

DOC-SOURCE is a symbol, a string, or a list of two strings which serves
as a citation for the information contained in DOCSTRING and ATTRS. Use
this if you copied such information from somewhere else, i.e. a
specification like the TEI P4 Guidelines. If DOC-SOURCE is a bound
symbol, it will be set to that symbol’s value as a variable. If
DOC-SOURCE is a string, it will be interpreted generically (adding
\"Information from \" followed by DOC-SOURCE to the docstrings of
relevant constructs), and if DOC-SOURCE is a list of two strings, it
will be interpreted as the URL of a publication followed by its title.
If DOC-SOURCE is not given, then the value of ‘hermeneus-doc-source’
will be used instead. If that value is nil (the default), then no
citation will appear in the relevant docstrings."
  ;; Note that I’m not sure if that last part actually works, urgh
  (declare (advertised-calling-convention
            (tag &optional docstring &key attrs face render doc-source &allow-other-keys) "")
           (indent defun)
           (doc-string 2)
           ;; The following debug spec doesn’t actually work, and
           ;; Edebug’s error messages on the matter are almost
           ;; Microsoftian in their opacity.
           ;; Let the record show that I tried.
           ;; (debug (&define name [&optional stringp]
           ;;                 &rest [&or [":render" def-body]
           ;;                            [keywordp &rest [&not keywordp]]]))
           )
  ;; Get the keywords
  (let* ((docstring (prog1 (when (stringp (car args)) (pop args))
                      (while (not (or (null (car args))
                                      (keywordp (car args))))
                        (pop args))))
         (kw-args (hermeneus--normalize-keywords args))
         (kw-vals (mapcar (lambda (x) (plist-get kw-args x))
                          hermeneus--tag-keywords)))
    ;; (small exception for ‘doc-source’—if it’s not given as an
    ;; argument, but ‘hermeneus-doc-source’ has a value outside the macro
    ;; call, use that value instead of ‘nil’)
    (when (and (not (plist-get kw-args :doc-source))
               (boundp 'hermeneus-doc-source))
      (let ((ds-pos (seq-position hermeneus--tag-keywords :doc-source)))
        (setf (elt kw-vals ds-pos) hermeneus-doc-source)))
    ;; Bind the keywords locally
    (cl-progv
        (mapcar
         (lambda (x) (thread-last x
                       (symbol-name)
                       (string-remove-prefix ":")
                       (intern)))
         hermeneus--tag-keywords)
        kw-vals
      ;; Finally, write out the definitions
      (let ((face-name (intern (format "hermeneus-face-%s" tag))))
        `(progn
           (defface ,face-name
             ,(if face
                  `,@face
                `'((t (:inherit hermeneus-default-face))))
             ,(hermeneus--doc-concat
               (format "Face used to render the XML tag ‘<%1$s>’.\n‘<%1$s>’ %2$s"
                       tag docstring)
               (hermeneus--doc-source doc-source))
             :tag ,(format "Hermeneus — face for XML tag <%s>" tag)
             :group 'hermeneus-faces)
           (defun ,(intern (format "hermeneus-render-%s" tag)) (dom)
             ,(hermeneus--doc-concat
               (format "Rendering function for the XML tag ‘<%1$s>’.\n‘<%1$s>’ %2$s"
                       tag docstring)
               (when attrs (concat "\n\nAttributes:"
                                   (hermeneus--format-attrs attrs)))
               (hermeneus--doc-source doc-source))
             ,@(if render
                   `,@(if (or (atom render) (atom (car render)))
                          (list render)
                        render)
                 (list `(hermeneus--render-generic dom ',tag ',face-name))))
           ;; (I know that ‘add-to-list’ is supposed to be used
           ;; sparingly in Lisp code, but the fact that ‘push’ would
           ;; add to the front of the list here is just too much for
           ;; my autistic sensibilities.)
           (add-to-list 'hermeneus-defined-tags ',tag t))))))

(let ((hermeneus-doc-source '("https://tei-c.org/Vault/P4/doc/html/CO.html" "Elements Available in All TEI Documents")))
(define-hermeneus-tag foreign
  "identifies a word or phrase as belonging to some language other than that of the surrounding text.")

(define-hermeneus-tag cit
  "A quotation from some other document, together with a bibliographic reference to its source.")

(define-hermeneus-tag name
  "contains a proper noun or noun phrase."
  :attrs
  type "indicates the type of the object which is being named by the phrase.")

(define-hermeneus-tag abbr
  "contains an abbreviation of any sort."
  :attrs
  expan "(expansion) gives an expansion of the abbreviation."
  resp "(responsibility) signifies the editor or transcriber responsible for supplying the expansion of the abbreviation held as the value of the expan attribute."
  type "allows the encoder to classify the abbreviation according to some convenient typology."
  cert "(certainty) signifies the degree of certainty ascribed to the expansion of the abbreviation.")

(define-hermeneus-tag date
  "contains a date in any format."
  :attrs
  calendar "indicates the system or calendar to which the date belongs."
  value "gives the value of the date in some standard form, usually yyyy-mm-dd."
  certainty "indicates the degree of precision to be attributed to the date.")

(define-hermeneus-tag pb
  "marks the boundary between one page of a text and the next in a standard reference system."
  :attrs
  ed "(edition) indicates the edition or version in which the page break is located at this point.")

(define-hermeneus-tag bibl
  "contains a loosely-structured bibliographic citation of which the sub-components may or may not be explicitly tagged."
  :render
  (let ((hermeneus--bibl-urn (hermeneus-urn-to-work (dom-attr dom 'n))))
    (hermeneus--render-generic dom)))

(define-hermeneus-tag biblScope
  "defines the scope of a bibliographic reference, for example as a list of pagenumbers, or a named subdivision of a larger work."
  :attrs
  type "identifies the type of information conveyed by the element, e.g. ‘pages’, ‘volume’.")

(define-hermeneus-tag title
  "contains the title of a work, whether article, book, journal, or series, including any alternative titles or subtitles."
  :attrs
  level "(bibliographic level (or class) of title) indicates whether this is the title of an article, book, journal, series, or unpublished material."
  type "(type of title) classifies the title according to some convenient typology."
  :render
  ;; a lot of this is copied from ‘author’ below; if anything else
  ;; copies this, refactoring may be in order
  (let* ((new-dom (copy-seq dom))
         (text (car (nthcdr (1- (safe-length new-dom)) new-dom)))
         (expansion (and (stringp text)
                         (gethash hermeneus--bibl-urn hermeneus-title-hash))))

    (when (and expansion hermeneus-expand-abbreviations)
      (setf (car (nthcdr (1- (safe-length new-dom)) new-dom)) expansion))

    (let ((start (point)))
      (hermeneus--render-generic new-dom)
      (when expansion
        (add-text-properties start (point)
                             (list 'help-echo
                                   (if hermeneus-expand-abbreviations
                                       text
                                     expansion)))))))

(define-hermeneus-tag author
  "in a bibliographic reference, contains the name of the author(s), personal or corporate, of a work; the primary statement of responsibility for any bibliographic item."
  :render
  (let* ((new-dom (copy-seq dom))
         (text (car (nthcdr (1- (safe-length new-dom)) new-dom)))
         (expansion (and (stringp text)
                         (gethash text hermeneus-author-abbr-hash))))

    (when (and expansion hermeneus-expand-abbreviations)
      (setf (car (nthcdr (1- (safe-length new-dom)) new-dom)) expansion))

    (let ((start (point)))
      (hermeneus--render-generic new-dom)
      (when expansion
        (add-text-properties start (point)
                             (list 'help-echo
                                   (if hermeneus-expand-abbreviations
                                       text
                                     expansion)))))))
)

(let ((hermeneus-doc-source '("https://tei-c.org/Vault/P4/doc/html/DI.html" "Print Dictionaries")))
(define-hermeneus-tag entryFree
  "contains a dictionary entry which does not necessarily conform to the constraints imposed by the entry element."
  :render
  (let* ((key (dom-attr dom 'key))
         (id (dom-attr dom 'id))
         (heading (dom-node nil nil (hermeneus-beta-to-unicode key))))
    (shr-ensure-paragraph)
    (shr-heading heading 'info-title-3)
    (shr-fontize-dom dom 'hermeneus-face-entryFree)
    (shr-ensure-paragraph))
  (setq hermeneus--prev-tag 'entryFree))

(define-hermeneus-tag sense
  "groups together all information relating to one word sense in a dictionary entry (definitions, examples, translation equivalents, etc.)"
  :attrs
  level "gives the nesting depth of this sense."
  :render
  (shr-ensure-newline)
  (let ((start (point))
        (start-pixel (hermeneus--pixel-column)))
    (let* ((bullet (shr-insert (concat (dom-attr dom 'n) ". "))) ; bullet is inserted, here
           (width (- (hermeneus--pixel-column) start-pixel))
           (margin (* (string-to-number (dom-attr dom 'level))
                      (hermeneus--margin-indent-width))))
      (shr-mark-fill start)
      (put-text-property start (1+ start)
                         'shr-continuation-indentation margin)
      (put-text-property start (1+ start) 'shr-indentation (- margin width))
      (shr-fontize-dom dom 'hermeneus-face-sense)))
  (unless (bolp)
    (insert "\n"))
  (setq hermeneus--prev-tag 'sense))

(define-hermeneus-tag gramGrp
  "groups morpho-syntactic information about a lexical item, e.g. pos, gen, number, case, or itype (inflectional class).")

(define-hermeneus-tag etym
  "encloses the etymological information in a dictionary entry.")

(define-hermeneus-tag xr
  "contains a phrase, sentence, or icon referring the reader to some other location in this or another text."
  :attrs
  type "indicates the type of cross reference, using any convenient typology.")

(define-hermeneus-tag orth
  "gives the orthographic form of a dictionary headword."
  :attrs
  type "gives the type of spelling."
  extent "gives the extent of the orthographic information provided.")

(define-hermeneus-tag pron
  "contains the pronunciation(s) of the word."
  :attrs
  extent "indicates whether the pronunciation is for whole word or part.")

(define-hermeneus-tag lbl
  "in dictionaries, contains a label for a form, example, translation, or other piece of information, e.g. abbreviation for, contraction of, literally, approximately, synonyms:, etc."
  :attrs
  type "classifies the label using any convenient typology.")

(define-hermeneus-tag gen
  "identifies the morphological gender of a lexical item, as given in the dictionary.")

(define-hermeneus-tag number
  "indicates grammatical number associated with a form, as given in a dictionary.")

(define-hermeneus-tag per
  "contains an indication of the grammatical person (1st, 2nd, 3rd, etc.) associated with a given inflected form in a dictionary.")

(define-hermeneus-tag tns
  "indicates the grammatical tense associated with a given inflected form in a dictionary.")

(define-hermeneus-tag mood
  "contains information about the grammatical mood of verbs (e.g. indicative, subjunctive, imperative)")

(define-hermeneus-tag itype
  "indicates the inflectional class associated with a lexical item."
  :attrs
  type "indicates the type of indicator used to specify the inflection class, when it is necessary to distinguish between the usual abbreviated indications (e.g. ‘inv’) and other kinds of indicators, such as special codes referring to conjugation patterns, etc.")

(define-hermeneus-tag pos
  "Indicates the part of speech assigned to a dictionary headword (noun, verb, adjective, etc.)")

(define-hermeneus-tag subc
  "contains subcategorization information (transitive/intransitive, countable/non-countable, etc.)")

(define-hermeneus-tag tr
  "contains a translation of the headword or an example.")
)

(let ((hermeneus-doc-source '("https://tei-c.org/Vault/P4/doc/html/ND.html" "Linking, Segmentation, and Alignment")))
(define-hermeneus-tag ref
  "defines a reference to another location in the current document, in terms of one or more identifiable elements, possibly modified by additional text or comment."
  :face
  '((t . (:inherit shr-link)))
  :render
  (if-let ((string1 (car (dom-strings dom)))
           
           ;; not sure how we will handle links to prefixes/suffixes,
           ;; so they are disabled for now
           ((not (string-prefix-p "-" (string-trim string1))))
           ((not (string-suffix-p "-" (string-trim string1))))
           
           (entries (hermeneus-get-entries hermeneus-lsj))
           (target (hermeneus--string-to-object string1)))
      (progn (hermeneus--insert-space-maybe)
             (let ((start (point)))
               (hermeneus--render-generic dom 'ref 'hermeneus-face-ref)
               (make-button start (point)
                            :type 'hermeneus-greek-word-button
                            'target target)))
    (hermeneus--render-generic dom 'ref 'hermeneus-default-face))
  :attrs
  target "specifies the destination of the reference by supplying the value of the id attribute on one or more other elements in the current document."
  type "categorizes the pointer in some respect, using any convenient set of categories.
Values: The type should indicate the intended function of the pointer, or the rhetorical relationship between its source and the target.
Default: #IMPLIED"
  resp "specifies the creator of the pointer.
Values: any string of characters, usually the initials or name of the creator.
Default: #IMPLIED"
  crdate "specifies when the pointer was created.
Values: A date in ISO 8601 format, generally yyyy-mm-dd.
Default: #IMPLIED"
  targType "specifies the kinds of elements to which this pointer may point.
Values: A list of valid element names declared in the DTD of the current document.
Default: #IMPLIED
Note: If this attribute is supplied, every element specified as a target must be of one or other of the types specified. An application may choose whether or not to report failures to satisfy this constraint as errors, but may not access an element of the right identifier but the wrong type."
  targOrder "where more than one identifier is supplied as the value of the target attribute, this attribute specifies whether the order in which they are supplied is significant.
Legal values are:
Y	Yes: the order in which IDREF values are specified as the value of a target attribute should be followed when combining the targeted elements.
N	No: the order in which IDREF values are specified as the value of a target attribute has no significance when combining the targeted elements.
U	Unspecified: the order in which IDREF values are specified as the value of a target attribute may or may not be significant.
Default: U"
  evaluate "specifies the intended meaning when the target of a pointer is itself a pointer.
Legal values are:
all	if the element pointed to is itself a pointer, then the target of that pointer will be taken, and so on, until an element is found which is not a pointer.
one	if the element pointed to is itself a pointer, then its target (whether a pointer or not) is taken as the target of this pointer.
none	no further evaluation of targets is carried out beyond that needed to find the element specified in the pointer's target.
Default: #IMPLIED
Note: If no value is given, the application program is responsible for deciding (possibly on the basis of user input) how far to trace a chain of pointers.")
)

(let ((hermeneus-doc-source '("https://tei-c.org/Vault/P4/doc/html/ND.html" "Names and Dates")))
(define-hermeneus-tag placeName
  "contains an absolute or relative place name.")
)

(provide 'hermeneus-tags)

;;; hermeneus-tags.el ends here
