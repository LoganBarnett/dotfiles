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
    (setq-default rainbow-identifiers-faces-to-override
                  '(
                    js2-object-property-access
                    js2-function-call
                    js2-object-property
                    font-lock-function-name-face
                    font-lock-variable-name-face
                    highlight-numbers-number
                    font-lock-constant-face
                    font-lock-keyword-face
                    )
                  )
    (add-hook 'org-mode-hook 'rainbow-mode)
    (add-hook 'css-mode-hook 'rainbow-mode)
    (add-hook 'prog-mode-hook 'rainbow-mode)
    ))
(provide 'config-rainbow-mode)

;;; config-rainbow-mode.el ends here
