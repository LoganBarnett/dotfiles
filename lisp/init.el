;;; init --- entry point for initializing the emacs config
;;; Commentary:
;; This is essentially the starting point for all of the Emacs config. Code can
;; go in .spacemacs, but that's harder to track since the file itself must
;; change over time during updates.

;;; Code:

(defun my/init ()
  "Do initializtion."
  (message "[INIT] Starting init.")
  (message "[INIT] private.org")
  (org-babel-load-file (expand-file-name "org/private.org"))
  (message "[INIT] whitespace.org")
  (org-babel-load-file (expand-file-name "org/whitespace.org"))
  (message "[INIT] habitica.org")
  (org-babel-load-file (expand-file-name "org/habitica.org"))
  (message "[INIT] Init Done.")
  )

(provide 'my/init)

;;; init.el ends here
