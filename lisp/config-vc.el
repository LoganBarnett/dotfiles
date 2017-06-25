;;; config-vc --- configure version-control support
;;; Commentary:

;;; Code:

;; configure vc
(defun config-vc ()
  "Configure version control."
  (use-package "git-gutter"
    ;; :init
    :config
    (setq-default git-gutter:update-interval 5)
  ))
(provide 'config-vc)

;;; config-vc.el ends here
