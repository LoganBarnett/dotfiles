;;; ox-confluence-en.el --- Enhanced Confluence Wiki Back-End for Org Export Engine

;; Copyright (C) 2015, Correl Roush

;; Author: Correl Roush <correl@gmail.com>
;; Keywords: outlines, confluence, wiki
;; Url: https://gist.github.com/correl/8347cd28b6f9218a1507

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; ox-confluence.el lets you convert Org files to confluence files
;; using the ox.el export engine.
;;
;; Put this file into your load-path and the following into your ~/.emacs:
;;	 (require 'ox-confluence-en)
;;
;; Export Org files to confluence:
;; M-x org-confluence-en-export-as-confluence RET
;;
;;; Code:
(require 'ox-confluence)

(defgroup org-export-confluence-en nil
  "Options for exporting to Confluence"
  :tag "Org Confluence"
  :group 'org-export)

(defcustom org-confluence-en-use-plantuml-macro t
  "Embed PlantUML graphs using the PlantUML macro.

When exporting the results of a PlantUML, dot, or ditaa source
block, use the Confluence PlantUML macro to render the graph
rather than linking to the resulting image generated locally.

Requires the free confluence PlantUML plugin to be installed:
https://marketplace.atlassian.com/plugins/de.griffel.confluence.plugins.plant-uml"
  :group 'org-export-confluence-en
  :type 'boolean)

(org-export-define-derived-backend 'confluence-en 'confluence
  :translate-alist '((headline . org-confluence-en-headline)
                     (paragraph . org-confluence-en-paragraph)
                     (src-block . org-confluence-en-src-block)
                     (quote-block . org-confluence-en-quote-block)
                     (special-block . org-confluence-en-special-block)
                     (verbatim . org-confluence-en-verbatim)
                     (code . org-confluence-en-verbatim)
                     (fixed-width . org-confluence-en-verbatim)
                     (table-cell . org-confluence-en-table-cell)
                     (item . org-confluence-en-item)
                     (timestamp . org-confluence-en-timestamp)))

(defun org-confluence-en-headline (headline contents info)
  (let* ((low-level-rank (org-export-low-level-p headline info))
	 (text (org-export-data (org-element-property :title headline)
				info))
	 (todo (org-export-data (org-element-property :todo-keyword headline)
				info))
	 (level (org-export-get-relative-level headline info))
	 (todo-text (if (or (not (plist-get info :with-todo-keywords))
			    (string= todo ""))
			""
                      (let* ((todo-type (org-element-property :todo-type headline))
                             (status-color (cond
                                            ((equal todo-type 'todo) "red")
                                            ((equal todo-type 'done) "grey")
                                            (t "green"))))
                        (format "%s " (org-confluence-en--macro "status" nil `((color . ,status-color) (title . ,todo))))))))
    ;; Else: Standard headline.
    (format "h%s. %s%s\n%s" level todo-text text
            (if (org-string-nw-p contents) contents ""))))

(defun org-confluence-en-paragraph (paragraph contents info)
  "Strip newlines from paragraphs.

Confluence will include any line breaks in the paragraph, rather
than treating it as reflowable whitespace."
  (replace-regexp-in-string "\n" " " contents))

(defun org-confluence-en-src-block (src-block contents info)
  "Embed source block results using available macros.

Currently, PlantUML, Graphviz, and Ditaa graphs will be embedded
using the macros provided by the PlantUML plugin, if it is
available."
  (let ((lang (org-element-property :language src-block))
        (code (org-export-format-code-default src-block info))
        (caption (if (org-export-get-caption src-block)
                     (org-trim (org-export-data (org-export-get-caption src-block) info)))))
    (if (and org-confluence-en-use-plantuml-macro
             (member lang '("plantuml" "dot" "ditaa")))
        (org-confluence-en--macro "plantuml" code `((type . ,lang)))
      (org-confluence-en--block lang "Emacs" caption code))))

(defun org-confluence-en--block (language theme caption contents)
  (concat "\{code:theme=" theme
          (when language (format "|language=%s" language))
          (when caption (format "|title=%s" caption))
          "}\n"
          contents
          "\{code\}\n"))

(defun org-confluence-en-quote-block (quote-block contents info)
  (org-confluence-en--macro "quote" contents))

(defun org-confluence-en-special-block (special-block contents info)
  (let ((block-type (downcase (org-element-property :type special-block))))
    (if (member block-type '("info" "note" "warning"))
        (org-confluence-en--macro block-type contents)
      (org-ascii-special-block special-block contents info))))

(defun org-confluence-en-verbatim (verbatim contents info)
  (format "{{%s}}"
          (org-trim (org-element-property :value verbatim))))

(defun org-confluence-en--macro (name contents &optional arguments)
  (let ((open-tag (concat "\{" name
                          (when arguments
                            (concat ":"
                                    (mapconcat (lambda (pair) (format "%s=%s"
                                                                      (car pair)
                                                                      (cdr pair)))
                                               arguments
                                               "|")))
                          "}"))
        (close-tag (concat "{" name "}")))
    (if contents (concat open-tag "\n" contents "\n" close-tag)
      open-tag)))

(defun org-confluence-en-export-as-confluence
    (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let ((org-babel-default-header-args:plantuml '((:exports . "code")))
        (org-babel-default-header-args:dot '((:exports . "code")))
        (org-babel-default-header-args:ditaa '((:exports . "code"))))
    (org-export-to-buffer 'confluence-en "*org CONFLUENCE Export*"
      async subtreep visible-only body-only ext-plist (lambda () (text-mode)))))

(defun org-confluence-en-table-cell  (table-cell contents info)
  "Wrap table cell contents in whitespace.

Without the extra whitespace, cells will collapse together thanks
to confluence's table header syntax being multiple pipes."
  (let ((table-row (org-export-get-parent table-cell)))
    (concat
     (when (org-export-table-row-starts-header-p table-row info)
       "|")
     " " contents " |")))

(defun org-confluence-en--checkbox (item info)
  "Return checkbox string for ITEM or nil.
INFO is a plist used as a communication channel."
  (cl-case (org-element-property :checkbox item)
    (on "☑ ")
    (off "☐ ")
    (trans "☒ ")))

(defun org-confluence-en-item (item contents info)
  (let ((list-type (org-element-property :type (org-export-get-parent item)))
        (checkbox (org-confluence-en--checkbox item info))
        (depth (1+ (org-confluence--li-depth item))))
    (cl-case list-type
      (descriptive
       (concat (make-string depth ?-) " "
               (org-export-data (org-element-property :tag item) info) ": "
               (org-trim contents)))
      (ordered
       (concat (make-string depth ?#) " "
               (org-trim contents)))
      (t
       (concat (make-string depth ?-)
               " "
               (org-trim contents))))))

(defun org-confluence-en-timestamp (timestamp contents info)
  (s-replace-all
   '(("[" . "")
     ("]" . ""))
   (org-ascii-timestamp timestamp contents info)))

(provide 'ox-confluence-en)
;;; ox-confluence-en ends here
