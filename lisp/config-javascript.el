;;; config-javascript --- configure elm support
;;; Commentary:
;; the layer sets up a lot of this

;;; Code:
(require 'flycheck)
(require 'rainbow-identifiers)
(require 'use-package)
(defvar module-directory)

(defun my/use-checker-from-node-modules (checker-name bin-name)
  "Make flycheck look for an executable for CHECKER-NAME inside the node_modules directory for a project."
  (defvar-local path "invalid")
  (setq-local path "invalid")
  ;; (message "flycheck -- setting %s exec for mode %s" checker-name major-mode )
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (path (and root
                     (expand-file-name (concat "node_modules/.bin/" bin-name)
                                       root))))
    (if path
        (let ((checker-exec-sym (intern (concat "flycheck-javascript-" checker-name "-executable"))))
             (make-local-variable checker-exec-sym)
             (set checker-exec-sym path)
             (message "exec %s is %s" checker-name path)
             )
      ;; (message "flycheck -- checker %s not available for mode %s with file %s"
      ;;          checker-name major-mode buffer-file-name)
      )
    )
  )

;; I don't get how this can work yet - need time to grok - unused
(defun my/use-node-modules-bin ()
  "Set executables of JS checkers from local node modules."
  (defvar-local file-name "")
  (defvar-local root "")
  (defvar-local module-directory "")
  (message "using node_modules/.bin for JS local linting/checking")
  (-when-let* ((file-name (buffer-file-name))
               (root (locate-dominating-file file-name "node_modules"))
               (module-directory (expand-file-name "node_modules" root)))
    (pcase-dolist (`(,checker . ,module) '((javascript-jshint . "jshint")
                                           (javascript-eslint . "eslint")
                                           (javascript-jscs   . "jscs")
                                           (javascript-flow   . "flow")
                                           (javascript-flow-coverage . "flow")))
      (let ((package-directory (expand-file-name module module-directory))
            (executable-var (flycheck-checker-executable-variable checker)))
        (when (file-directory-p package-directory)
          (set (make-local-variable executable-var)
               (expand-file-name (concat ".bin/" module)
                                 package-directory)))))))

(defun my/fix-js2-rainbow-identifiers ()
  "Plea to the gods to fix rainbow-identifiers with js2-mode."
  (message "HACK: turning off rainbow-identifiers-mode")
  (rainbow-identifiers-mode 0)
  (message "HACK: turning back on rainbow-identifiers-mode")
  (rainbow-identifiers-mode 1)
  )

(defun my/js2-disable-global-variable-highlight ()
  "Disable js2 global variable highlight.  Wait.  Am I using this?"
  (font-lock-remove-keywords 'js2-mode 'js2-external-variable)
  )

;; configure javascript
(defun config-javascript ()
  "Configure Javascript."
  (defvar grep-find-ignored-directories)
  (use-package "js2-mode"
  :init
  (require 'grep)
  (require 'nvm)
  ;; (require 'flow-minor-mode)
  :config
  ;; (add-hook 'js2-mode-hook 'flow-minor-enable-automatically)
  (nvm-use "8.1.3")
  (add-to-list 'grep-find-ignored-directories "node_modules")
  (setq-default js-indent-level 2)
  (setq-default js2-strict-missing-semi-warning nil)
  (setq-default js2-strict-trailing-comma-warning nil)
  (setq-default js2-mode-show-parse-errors nil)
  (setq-default js2-highlight-external-variables nil)
  ;; (setq-default js2-mode-toggle-warnings-and-errors 0)
  (setq-default js2-mode-show-strict-warnings nil)
  ;; (add-hook 'js2-mode 'js2-mode-toggle-warnings-and-errors)
  ;; (add-hook 'js2-mode 'my/disable-js2-global-var-highlight)

  ;; prevent indentation from lining up with a prior line's glyph
  ;; this will make it so fighting is less necessary to appease linters
  (setq-default js2-pretty-multiline-declarations nil)
  )
  (message "configured javascript support")
  )
(provide 'config-javascript)

;;; config-javascript.el ends here
