;;; config-rainbow-mode --- configure elm support
;;; Commentary:
;; the layer sets up a lot of this

;;; Code:
(require 'use-package)

;; configure rainbow-mode
(defun config-rainbow-mode ()
  "Configure rainbow-mode."
  (use-package "rainbow-mode"
    ;; :init
    :config
    (add-hook 'org-mode-hook 'rainbow-mode)
    (add-hook 'css-mode-hook 'rainbow-mode)
    (add-hook 'prog-mode-hook 'rainbow-mode)
    ))
(provide 'config-rainbow-mode)

;;; config-rainbow-mode.el ends here
