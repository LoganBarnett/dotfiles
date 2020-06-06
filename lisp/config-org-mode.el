;;; config-org-mode --- configure org support
;;; Commentary:
;; Configure org-mode to my liking.
;;; Code:
(require 'use-package)

;; configure org-mode
(defun config-org-mode ()
  "Configure 'org-mode'."
  ;; (package-initialize)
  (use-package "org"
  ;;   :requires (
  ;;              ;; Cover some languages we want supported.
  ;;              ob-js
  ;;              ob-sh
  ;;              ob-plantuml
  ;;              ;; Exporters.
  ;;              ox-confluence-en ;; Adds PlantUML support to Confluence exports.
  ;;              ox-gfm ;; Github Flavored Markdown.
  ;;              )
    ;; :ensure org-plus-contrib
    :init
    :config
    )
)

(provide 'config-org-mode)

;;; config-org-mode.el ends here
