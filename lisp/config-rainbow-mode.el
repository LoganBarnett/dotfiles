;;; config-rainbow-mode --- configure rainbow-mode support
;;; Commentary:
;; rainbow-mode highlights CSS color names and expressions (short, long, rgba,
;; etc) with the color indicated.

;;; Code:
(require 'use-package)

;; configure rainbow-mode
(defun config-rainbow-mode ()
  "Configure rainbow-mode."
  (use-package "rainbow-mode"
    ;; :init
    :config
    (add-hook 'css-mode-hook 'rainbow-mode)
    (add-hook 'prog-mode-hook 'rainbow-mode)
    ))
(provide 'config-rainbow-mode)

;;; config-rainbow-mode.el ends here
