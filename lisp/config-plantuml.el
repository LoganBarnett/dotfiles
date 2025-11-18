;;; config-plantuml --- configure plantuml support
;;; Commentary:
;; the layer sets up a lot of this

;;; Code:

(require 'use-package)

;; configure plantuml
(defun config-plantuml ()
  "Configure Plantuml."
  (interactive)

  (use-package "plantuml-mode"
    :config
    (setq-default plantuml-exec-mode 'executable)
    ;; (setq-default plantuml-jar-path
    ;;               "/usr/local/opt/plantuml/libexec/plantuml.jar")
    ;; (setq-default org-plantuml-jar-path
    ;;               "/usr/local/opt/plantuml/libexec/plantuml.jar")
    (setq-default plantuml-java-args "java.awt.headless=true")
    )

  (use-package ob-plantuml
    :config
    (setq-default org-plantuml-exec-mode 'plantuml)
    )

  )

(provide 'config-plantuml)

;;; config-plantuml.el ends here
