;;; hermeneus-cts.el --- -*- lexical-binding: t -*-

;; [[id:TKR:381cf97b-9b75-471a-a25c-eb82f16d3417][Variables:1]]
(defvar hermeneus-scaife-api-url "http://scaife-cts.perseus.org/api/cts"
  "The URL for the Perseus Project’s Scaife CTS API.")
;; Variables:1 ends here

;; [[id:TKR:263e9918-cdaf-4218-a60a-f39fc095324b][Convert URNs:1]]
(defun hermeneus-urn-to-base (urn)
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

(defun hermeneus-urn-to-work (urn)
  "Return URN shortened to the work part of the work component.
E.g., “urn:cts:greekLit:tlg0020.tlg001.perseus-grc1:195” will be
returned as “urn:cts:greekLit:tlg0020.tlg001”.
URN should be in the Canonical Text Services URN format. See
https://github.com/cite-architecture/ctsurn_spec/blob/master/md/specification.md"
  (setq urn (hermeneus-urn-to-base urn))
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

(defun hermeneus-urn-to-url (urn &optional atom)
  "Return the canonical Perseus Catalog URL for URN.
If ATOM is non-nil, return the URL for the Atom version."
  (concat "http://data.perseus.org/catalog/" (hermeneus-urn-to-base urn)
          (when atom "/atom")))
;; Convert URNs:1 ends here

(provide 'hermeneus-cts)

;;; hermeneus-cts.el ends here
