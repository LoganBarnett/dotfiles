;;; ibuffer-config.el --- configure ibuffer with evil-collection -*- lexical-binding: t; -*-
;;; Commentary:
;; ibuffer is Emacs' built-in buffer management interface.  Configure it with
;; evil-collection for vim-like keybindings.

;;; Code:
(require 'use-package)

(use-package ibuffer
  :config
  ;; Initialize evil-collection for ibuffer to get vim-like keybindings.
  ;; Only initialize if evil-collection is available.
  (with-eval-after-load 'evil-collection
    (evil-collection-init 'ibuffer))

  ;; Add Doom's minimal keybindings.
  (with-eval-after-load 'general
    (map! :map ibuffer-mode-map
          :n "q" #'kill-current-buffer))
  )

(provide 'ibuffer-config)
;;; ibuffer-config.el ends here
