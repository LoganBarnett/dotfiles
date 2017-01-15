;;; config-java --- configure java support
;;; Commentary:
;; uses eclimd (https://github.com/emacs-eclim/emacs-eclim) to provide IDE-like
;; java support. Has company completion kind of.

;;; Code:

;; configure java
(defun config-java ()
  (use-package "eclim"
    :init
    ;; (paradox-require "flycheck-java")
    :config
    (setq eclimd-autostart t)
    (global-eclim-mode)
    )
  )
(provide 'config-java)

;;; config-java.el ends here
