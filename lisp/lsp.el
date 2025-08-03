
;;; Code:

(require 'use-package)

(use-package "lsp-mode"
  :init
  :config
  ;; Show us more complete and helpful information under the cursor.
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-eldoc-render-all t)          ;; show all info, not truncated
  (setq lsp-idle-delay 0.2)              ;; faster response
  (setq lsp-eldoc-enable-hover t)        ;; enable hover in minibuffer
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
