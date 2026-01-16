;;; Code:

(require 'use-package)

(use-package just-ts-mode
  :init
  (setq-default just-indent-offset 2)
  :config
  (setq-local tab-width 2)
  )
