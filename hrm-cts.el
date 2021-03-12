;;; hrm-cts.el --- -*- lexical-binding: t -*-

(defvar hrm-scaife-api-url "http://scaife-cts.perseus.org/api/cts"
  "The URL for the Perseus Project’s Scaife CTS API.")

(defun hrm-urn-to-base (urn)
  "Return URN with the passage component, if present, removed.
E.g., “urn:cts:greekLit:tlg0020.tlg001.perseus-grc1:195” will be
returned as “urn:cts:greekLit:tlg0020.tlg001.perseus-grc1”.
URN should be in the Canonical Text Services URN format. See
https://github.com/cite-architecture/ctsurn_spec/blob/master/md/specification.md"
  (save-match-data
    (if (and urn
             (string-match
              (rx (= 4 (one-or-more
                        (not (any control ":" "\\" "\"" "&" "<" ">" "^" "`" "|" "{" "}" "~")))
                     ":"))
              urn))
        (substring urn 0 (1- (match-end 0)))
      urn)))

(defun hrm-urn-to-work (urn)
  "Return URN shortened to the work part of the work component.
E.g., “urn:cts:greekLit:tlg0020.tlg001.perseus-grc1:195” will be
returned as “urn:cts:greekLit:tlg0020.tlg001”.
URN should be in the Canonical Text Services URN format. See
https://github.com/cite-architecture/ctsurn_spec/blob/master/md/specification.md"
  (setq urn (hrm-urn-to-base urn))
  (save-match-data
    (if (and urn (string-match
                  (rx (= 3 (one-or-more
                            (not (any control ":" "\\" "\"" "&" "<" ">" "^" "`" "|" "{" "}" "~")))
                         ":")
                      (= 2 (one-or-more
                            (not (any control "." "\\" "\"" "&" "<" ">" "^" "`" "|" "{" "}" "~")))
                         "."))
                  urn))
        (substring urn 0 (1- (match-end 0)))
      urn)))

(defun hrm-urn-to-url (urn &optional atom)
  "Return the canonical Perseus Catalog URL for URN.
If ATOM is non-nil, return the URL for the Atom version."
  (concat "http://data.perseus.org/catalog/" (hrm-urn-to-base urn)
          (when atom "/atom")))

(provide 'hrm-cts)

;;; hrm-cts.el ends here
