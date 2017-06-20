;;; config-so-long-mode --- configure so-long support
;;; Commentary:
;; Configure so-long-mode to my liking.

;;; Code:
(require 'use-package)
(require 'so-long)
(defvar so-long-minor-modes)
(defvar so-long-target-modes)

;; configure so-long-mode
(defun config-so-long-mode ()
  "Configure so-long-mode."
  (use-package "so-long"
    ;; :init
    :config
    (add-to-list 'so-long-minor-modes 'rainbow-delimiters-mode)
    (add-to-list 'so-long-minor-modes 'rainbow-mode)
    ;; (add-to-list 'so-long-minor-modes 'color-identifiers-mode)
    (add-to-list 'so-long-minor-modes 'rainbow-identifiers-mode)
    (add-to-list 'so-long-minor-modes 'flycheck-mode)
    (add-to-list 'so-long-target-modes 'json-mode)
    (so-long-enable)
    (setq-default so-long-threshold 500)
    (message "so-long watching enabled")
    ))
(provide 'config-so-long-mode)

;;; config-so-long-mode.el ends here
