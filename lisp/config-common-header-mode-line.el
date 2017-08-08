;;; config-common-header-mode-line --- configure common-header-mode-line support
;;; Commentary:

;;; Code:

;; (require 'use-package)
;; configure common-header-mode-line
(defun config-common-header-mode-line ()
  "Configure Common-Header-Mode-Line."

  ;; https://github.com/m2ym/popwin-el
  (require 'popwin)
  (defvar popwin:special-display-config)
  
  (defvar-local original-buffer (current-buffer))
  (push '("*header-bar*" :noselect t :stick t :height 1)
        popwin:special-display-config)
  (defvar-local header-bar-buffer (get-buffer-create "*header-bar*"))
  (popwin:display-buffer header-bar-buffer)
  (switch-to-buffer header-bar-buffer)
  (insert "foo bar")
  (switch-to-buffer original-buffer)


  ;; (require 'mode-line-frame)
  ;; (use-package "common-header-mode-line"
  ;; :init
  ;; :config
  ;; (require 'common-header-mode-line)
  ;; (require 'per-frame-header-mode-line)
  ;; (with-eval-after-load "common-header-mode-line-autoloads"
  ;; (message "activating common-header-mode-line-mode")
  ;; (common-mode-line-mode 1)
  ;; (common-header-mode-line-mode 1)
  ;; )
  ;; )

  ;;; old aborted code:

  ;; (with-current-buffer (generate-new-buffer "*empty*")

  ;; (make-frame '((minibuffer . nil)
  ;;               (unsplittable . t)
  ;;               (buffer-predicate . (lambda (x) nil))
  ;;               (height . 2)
  ;;               (width . internal-border-width)
  ;;               (left-fringe . 0)
  ;;               (right-fringe . 0)
  ;;               (tool-bar-lines . 0)
  ;;               (menu-bar-lines . 0)))

  ;; (split-window-vertically)

  ;; Docs say special-display-buffer-names is deprecated/removed and
  ;; display-buffer-alist should be used instead. However, the docs don't
  ;; indicate how to replicate the behavior of special-display-buffer-names.
  ;; popwin:special-display-config has some promise but also includes actual
  ;; configuration for how the handle the buffer. I need to look into this more.

  ;; (add-to-list display-buffer-alist "*header-bar*")
  ;; (add-to-list popwin:special-display-config )
  ;; (add-to-list special-display-buffer-names "*header-bar*")
  ;; (split-window)

  ;; (add-to-list popwin:special-display-config ("*header-bar*"
  ;;                                             :noselect t
  ;;                                             :stick t
  ;;                                             ))

  ;; (append-to-buffer header-bar)
  ;; (popwin:create-popup-window ("*header-bar*" :noselect t :stick t))
    
  ;;   (set-window-dedicated-p
  ;;    (get-buffer-window (current-buffer) t) t))

  )

(provide 'config-common-header-mode-line)
;;; config-common-header-mode-line.el ends here
