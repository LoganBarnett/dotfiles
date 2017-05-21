;;; org-to-jekyll --- export org files to something jekyll can use
;;; Commentary:
;; org-to-jekyll exports org files in a format pleasing to jekyll for blog
;; posts.

;;; Code:
(defun org-to-jekyll ()
  "Export an org file to a jekyll friendly file."
  (interactive)
  (setq-local org-export-with-toc nil)
  (org-html-export-to-html nil nil nil t)
  )
(provide 'org-to-jekyll)
;;; org-to-jekyll.el ends here
