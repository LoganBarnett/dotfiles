;;; config-typescript --- configure typescript support
;;; Commentary:
;; the layer sets up a lot of this

;;; Code:
(require 'use-package)
(defvar flycheck-current-errors)
(defvar typescript-indent-level)

(defun is-passive-aggressive-import-error (error)
  "Is t if flycheck-error ERROR is passive aggressive opinionation."
  (let ((matchingMessage "An import path cannot end with a"))
    (let ((partialMessage (substring
                           (flycheck-error-message error)
                           0 (length matchingMessage) )))
    (string= partialMessage matchingMessage)
    ))
  )

;; The typescript team has opted to put into Typescript's module resolution firm
;; opinionation regarding modules vs. files in the JavaScript ecosystem. Webpack
;; uses extensions to disambiguate files from each other, such as images and
;; code files since they all can be imported with the loader system. ts-loader
;; works great and knows how to resolve a .ts file properly, but flycheck still
;; reports the imports as errors. These errors do not show up as build errors,
;; so we're free to ignore them in Webpack based projects.
;;
;; Reading:
;; https://github.com/Microsoft/TypeScript/issues/9538
;; https://github.com/Microsoft/TypeScript/pull/9646
;; https://github.com/Microsoft/TypeScript/issues/10567
;;
;; TODO: Remove the fringes and underlines as well.
(defun flycheck-typescript-remove-passive-aggressive-import-errors ()
  "Remove passive aggressive error messages from flycheck."
  (setq flycheck-current-errors
        (cl-delete-if #'is-passive-aggressive-import-error
                   flycheck-current-errors))
  (flycheck-error-list-refresh)
  )
;; configure typescript
(defun config-typescript ()
  "Setup Typescript support."
  (use-package "tide"
    :init
    :config
    (require 'flycheck)
    (require 'cl-lib)
    (use-package "typescript-mode"
      :config
      (setq-default typescript-indent-level 2)
      )
    (add-hook 'flycheck-after-syntax-check-hook
              #'flycheck-typescript-remove-passive-aggressive-import-errors)
    (add-hook 'flycheck-error-list-after-refresh-hook
              #'flycheck-typescript-remove-passive-aggressive-import-errors)
    (flycheck-error-list-highlight-errors)
    )
  )
(provide 'config-typescript)

;;; config-typescript.el ends here
