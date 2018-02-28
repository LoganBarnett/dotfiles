;;; org-mode-auto-id-headlines --- Insert CUSTOM_ID properties into headlines.
;;; Commentary:
;; Automatically insert CUSTOM_ID into org-mode headlines on save. This makes
;; exporting headline links deterministic. Otherwise org-mode assigns a random
;; ID that changes on every run.
;;; Code:

(require 'org)

(defun add-friendly-headlines ()
  "Add friendly and deterministic ids to the current buffer."
  (save-excursion
    (widen)
    ;; (beginning-of-buffer)
    (goto-char (point-min))

    (org-element-map
      (org-element-parse-buffer 'headline)
      'headline
     (lambda (el)
       (let ((id (org-element-property :raw-value el)))
         (let ((path-id (string-join (heading-hierarchy-list el (list id)) "--")))
               (outline-next-heading)
               (org-entry-put (point) "CUSTOM_ID"
                              (anchorize-headline-title path-id))
               )
           )
     ))))

(defun heading-hierarchy-list (child hierarchy)
  "Recurse from CHILD to build a parent-first HIERARCHY list of headline titles."
  (let* ((parent (org-element-property :parent child))
        (parent-title (org-element-property :raw-value parent)))
    (if (and parent parent-title)
        (heading-hierarchy-list parent
                                (add-to-list 'hierarchy
                                             parent-title
                                             )
                                )
      hierarchy
      )
    )
  )

(defun anchorize-headline-title (title)
  "Convert TITLE to an HTML anchor-worthy name.
This is kebob case, with no quotes, spaces, or punctuation marks."
  (replace-regexp-in-string
   "\"\\|)\\|(\\|,\\|?" ""
   (downcase (replace-regexp-in-string " " "-" title))
   )
  )

;; Lifted from
;; http://kitchingroup.cheme.cmu.edu/blog/2013/05/05/Getting-keyword-options-in-org-files/
(defun get-org-keywords ()
  "Parse the buffer and return a cons list of (property . value)
from lines like:
#+PROPERTY: value

Property names are always lowercase in the returned structure."
  (org-element-map (org-element-parse-buffer 'element) 'keyword
    (lambda (keyword) (cons (org-element-property :key keyword)
                            (org-element-property :value keyword))))
  )

(defun get-org-keyword (keyword)
  "Get the value of a KEYWORD from the `org-mode' buffer.
KEYWORD is lowercase regardless of the document's value.
Given this in the document:
#+FOO: bar
This code:
\(get-org-keyword \"foo\"\)
Will yield:
\"bar\""
  (cdr (assoc keyword (get-org-keywords)))
  )

;; IDs for HTML anchors from exported org-mode docs are not deterministic nor
;; human friendly. Set the ID to be a derivation of the headline hierarchy.
(defun use-friendly-deterministic-headline-html-anchors ()
  "When saving an `org-mode' doc, add CUSTOM_IDs to all headlines in the doc."
  (require 'org-id)
  (setq-default org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  (add-hook 'before-save-hook
            (lambda ()
              (when (and (eq major-mode 'org-mode)
                         (eq buffer-read-only nil)
                         (not (eq (get-org-keyword "AUTO_ID") nil))
                         )
                (message "Adding auto ids to org buffer \"%s\"" (buffer-name))
                (add-friendly-headlines)
                ))
            )
  )

(defun org-mode-auto-id-headlines ()
  ""
  (use-friendly-deterministic-headline-html-anchors)
  )

(provide 'org-mode-auto-id-headlines)

;;; org-mode-auto-id-headlines ends here
