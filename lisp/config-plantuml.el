;;; config-plantuml --- configure plantuml support
;;; Commentary:
;; the layer sets up a lot of this

;;; Code:

(require 'use-package)

;; configure plantuml
(defun config-plantuml ()
  "Configure Plantuml."
  (use-package "plantuml-mode"
  ;; :init
  :config
  (setq-default plantuml-jar-path
                "/usr/local/opt/plantuml/libexec/plantuml.jar")
  (setq-default org-plantuml-jar-path
                "/usr/local/opt/plantuml/libexec/plantuml.jar")
  (setq-default plantuml-java-args "java.awt.headless=true")
  ))
(provide 'config-plantuml)

;;; config-plantuml.el ends here
