
;;; Code:

(require 'use-package)

(use-package "lsp-mode"
  :init
  :config
  (map!
   :mode lsp-mode
   :localleader
   (:prefix
    ("c" . "code")
    :desc "lsp-action" "a" #'lsp-execute-code-action
    )
   )
  )
(provide 'lsp)
;;; lsp.el ends here
