;;; config-whitespace --- configure whitespace support
;;; Commentary:
;; This package also includes some settings around whitespace related things
;; such as fci-mode and highlighting text that exceeds 80 columns.
;;; Code:

(require 'use-package)

;; old config

;; highlight lines longer than 80 chars
;; (require 'whitespace)
;; (setq whitespace-style '(tabs face empty lines-tail trailing))
;; (global-whitespace-mode t)
;; taken from https://www.emacswiki.org/emacs/EightyColumnRule
;; (add-hook 'font-lock-mode-hook #'my/highlight-gt-80-columns)
;; (add-hook 'prog-mode-hook #'my/highlight-gt-80-columns)
;; (add-hook 'text-mode-hook #'my/highlight-gt-80-columns)

(defun my/highlight-gt-80-columns ()
  "Highlight any text exceeding 80 columns.  You naughty text, you."
  (require 'font-lock)
  (defface my-tab-face '((t . (:background "gray10"))) "wide line tab face")
  ;; TODO: figure out why this breaks rainbow identifiers
  ;; (defface my-long-line-face '((t . (:background "gray10"))) "wide line face")
  (defface my-trailing-space-face '((t . (:background "red"))) "trailing space")
  (defface my-post-long-line-face '((t . (:underline "red"))) "post 80 face")

  (font-lock-add-keywords nil
                          '(("\t+" (0 'my-tab-face append))
                            ("[ \t]+$"      (0 'my-trailing-space-face append))
                            ;; ("^.\\{81,\\}$" (0 'my-long-line-face append))
                            ("^.\\{80\\}\\(.+\\)$" (1 'my-post-long-line-face append))
                            )
                          )
  (message "applied > 80 column highlighting")
  )
;; configure whitespace
(defun config-whitespace ()
  "Configure Whitespace."
  (use-package "whitespace"
  :init
  :config
  (setq-default whitespace-style '(face
                                   lines-tail
                                   space-before-tab
                                   space-after-tab))
  (add-hook 'prog-mode-hook 'whitespace-mode)
  ;; (global-whitespace-mode nil)
  ;; show 80 column rule
  (require 'fill-column-indicator)
  ;; (define-globalized-minor-mode global-fci-mode
  ;;   fci-mode (lambda ()
  ;;              (when (not (memq major-mode
  ;;                               (list 'web-mode)))
  ;;                (fci-mode 1))))
  ;; (global-fci-mode 1)
  (add-hook 'prog-mode-hook 'fci-mode)
  (add-hook 'text-mode-hook 'fci-mode)
  (add-hook 'web-mode-hook (lambda () (fci-mode 0)))


  ;; This prevents fci-mode from inserting junk characters at the ends of lines
  ;; during an export. The characters for me usually would show up as: "",
  ;; which doesn't seem to render in emacs. These are character codes 57345 and
  ;; 57344 (which vary based on what is seen in the document vs what is
  ;; exported?). See more about this here:
  ;; https://github.com/alpaker/Fill-Column-Indicator/issues/45 which also
  ;; includes the nice solution below.
  (defun fci-mode-override-advice (&rest args))
  (advice-add 'org-html-fontify-code :around
              (lambda (fun &rest args)
                (advice-add 'fci-mode :override #'fci-mode-override-advice)
                (let ((result  (apply fun args)))
                  (advice-remove 'fci-mode #'fci-mode-override-advice)
                  result)))
  ;; (add-hook 'buffer-list-update-hook 'turn-on-fci-mode)
  )
  )
(provide 'config-whitespace)

;;; config-whitespace.el ends here
