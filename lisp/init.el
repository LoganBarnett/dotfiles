;;; init --- entry point for initializing the emacs config
;;; Commentary:
;; This is essentially the starting point for all of the Emacs config. Code can
;; go in .spacemacs, but that's harder to track since the file itself must
;; change over time during updates.

;;; Code:

(defun init-org-file (file)
  "Logs FILE before it loading a file to help with debugging init issues."
  (message "[INIT] %s" file)
  (org-babel-load-file (expand-file-name (format "org/%s" file) "~/dev/dotfiles"))
  )

(defun my/init ()
  "Do initializtion."
  (message "[INIT] Starting init.")
  (init-org-file "private.org")
  (init-org-file "buffer.org")
  (init-org-file "whitespace.org")
  (init-org-file "habitica.org")
  (init-org-file "javascript.org")
  (init-org-file "groovy.org")
  (init-org-file "purescript.org")
  (init-org-file "css.org")
  (init-org-file "makefile.org")
  (message "[INIT] Init Done.")
  )

(provide 'my/init)

;;; init.el ends here
