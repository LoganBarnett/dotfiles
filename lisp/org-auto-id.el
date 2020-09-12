;;; org-auto-id --- Insert CUSTOM_ID properties into headlines.
;;; Commentary:
;; Automatically insert CUSTOM_ID into org-mode headlines on save. This makes
;; exporting headline links deterministic. Otherwise org-mode assigns a random
;; ID that changes on every run.
;;
;; This can be enabled by adding `#+auto_id: t' to the headers of your org-mode
;; file.
;;
;;; Code:

(require 'org)

(defmacro org-auto-id/without-undo (&rest body)
  "Disable undo for BODY.

org-auto-id can potentially write a lot of changes to the buffer.
Storing each of these changes as an undo point clutters the undo
buffer and undoing each edit is not a desired behavior."
  `(progn
    (buffer-disable-undo)
    (with-demoted-errors "Error during org-auto-id: %s" ,@body)
    (buffer-enable-undo)
    )
  )

(defun org-auto-id/id-as-extra-kebab (hierarchy-list)
  "Convert HIERARCHY-LIST to kebab-case, with extra \"-\" between headings.

For example using the hierarchy foo -> bar -> baz qux with foo
being at the top of the hierachy and baz qux being at the bottom.
The output would be:

\"foo--bar--baz-qux\""
  (org-auto-id/anchorize-headline-title (string-join hierarchy-list "--"))
  )

(defun org-auto-id/buffer-custom-id-populate ()
  "Add friendly and deterministic ids to the current buffer.

IDs for HTML anchors from exported `org-mode' buffers are not
deterministic nor human friendly. By default sets the CUSTOM_ID
to be a derivation of the headline hierarchy. The CUSTOM_ID is
then used during the export process to set the HTML anchor. Set
the buffer's AUTO_ID_FN to the symbol of a function in order to
customize the generated CUSTOM_ID value. The function must accept
an org heading heading heirarchy from
`org-auto-id/heading-hierarchy-list' and return the string to be
used for the CUSTOM_ID.

See `org-auto-id/id-as-extra-kebab' for the default AUTO_ID_FN.
The case of AUTO_ID_FN does not matter.

To override AUTO_ID_FN put this at the top of your buffer:

#+AUTO_ID_FN: my-fancy-auto-id-fn


If CUSTOM_ID is already set for a given heading then it will be overwritten."
  (interactive)
  (require 'org-id)
  (setq-local org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (message "disabling undo")
  (org-auto-id/without-undo
   (message "undo disabled")
   (save-excursion
     (widen)
     ;; (beginning-of-buffer)
     (goto-char (point-min))

     (let ((format-to-id (or (intern-soft
                              (org-auto-id/get-org-keyword "AUTO_ID_FN"))
                             'org-auto-id/id-as-extra-kebab
                             )))
       (message "format-to-id %s" format-to-id)
       (org-element-map
           (org-element-parse-buffer 'headline)
           'headline
         (lambda (el)
           (let ((id (org-element-property :raw-value el)))
             (outline-next-heading)
             (org-entry-put (point) "CUSTOM_ID"
                            (funcall format-to-id
                                     (org-auto-id/heading-hierarchy-list
                                      el
                                      (list id)))
                            )
             )
           )
       )))))

(defun org-auto-id/heading-hierarchy-list (child hierarchy)
  "Recurse from CHILD to build a parent-first HIERARCHY list of headline titles."
  (let* ((parent (org-element-property :parent child))
        (parent-title (org-element-property :raw-value parent)))
    (if (and parent parent-title)
        (org-auto-id/heading-hierarchy-list parent
                                (add-to-list 'hierarchy
                                             parent-title
                                             )
                                )
      hierarchy
      )
    )
  )

(defun org-auto-id/anchorize-headline-title (title)
  "Convert TITLE to an HTML anchor-worthy name.
This is kebob case, with no quotes, spaces, or punctuation marks."
  (replace-regexp-in-string
   "\"\\|)\\|(\\|,\\|?" ""
   (downcase (replace-regexp-in-string " " "-" title))
   )
  )

;; Lifted from
;; http://kitchingroup.cheme.cmu.edu/blog/2013/05/05/Getting-keyword-options-in-org-files/
;; TODO: It would be great to get these locally from the hierarachy and work our
;; way up.
(defun org-auto-id/get-org-keywords ()
  "Parse the buffer and return a cons list of (property . value)
from lines like:
#+PROPERTY: value

Property names are always lowercase in the returned structure."
  (org-element-map (org-element-parse-buffer 'element) 'keyword
    (lambda (keyword) (cons (org-element-property :key keyword)
                            (org-element-property :value keyword))))
  )

(defun org-auto-id/get-org-keyword (keyword)
  "Get the value of a KEYWORD from the `org-mode' buffer.
KEYWORD is lowercase regardless of the document's value.
Given this in the document:
#+FOO: bar
This code:
\(org-auto-id-get-org-keyword \"foo\"\)
Will yield:
\"bar\""
  (cdr (assoc keyword (org-auto-id/get-org-keywords)))
  )

(defun org-auto-id/on-save-auto-id ()
  "When saving an `org-mode' buffer, add CUSTOM_IDs to all headlines.

See `org-auto-id/buffer-custom-id-populate' for details and
customization options."
  (interactive)
  (add-hook 'before-save-hook
            (lambda ()
              (when (and (eq major-mode 'org-mode)
                         (eq buffer-read-only nil)
                         ;; This isn't working properly and should probably
                         ;; check for "nil" (string) as well.
                         (not (eq (org-auto-id/get-org-keyword "AUTO_ID") nil))
                         )
                (message "Adding auto ids to org buffer \"%s\"" (buffer-name))

                (org-auto-id/buffer-custom-id-populate)
                ))
            )
  )

(provide 'org-auto-id)

;;; org-auto-id ends here
