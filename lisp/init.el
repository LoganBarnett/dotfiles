;;; init --- entry point for initializing the emacs config
;;; Commentary:
;; This is essentially the starting point for all of the Emacs config. Code can
;; go in .spacemacs, but that's harder to track since the file itself must
;; change over time during updates.

;;; Code:

(defun init-org-file (file)
  "Logs FILE before it loading a file to help with debugging init issues."
  (message "[INIT] %s" file)
  (org-babel-load-file (expand-file-name (format "org/%s" file) "~/dev/dotfiles"))
  )

;; flycheck
(defun my/init-flycheck ()
  "Setup flycheck to my liking."
  (use-package "flycheck"
    ;; :defer t
    :ensure t
    :init
    ;; turn on flychecking globally
    ;; (add-hook 'after-init-hook #'global-flycheck-mode)
    ;; (add-hook 'js-mode-hook 'flycheck-mode)
    ;; (add-hook 'prog-mode #'flycheck-mode)
    (add-hook 'prog-mode-hook #'flycheck-mode)
    (setq-default syntax-checking-enable-by-default t)
    :config

    ;; node-modules support shamelessly lifted from here
    ;; https://github.com/lunaryorn/.emacs.d/blob/master/lisp/lunaryorn-flycheck.el#L62
    ;; (add-hook 'flycheck-mode-hook #'my/use-node-modules-bin)
    ;; can't use flycheck-syntax-check-failed-hook because it's for
    ;; when flycheck itself has an error
    ;; TODO: As of emacs 25 there's some huge bugginesss with automatically showing errors
    ;; (add-hook 'flycheck-after-syntax-check-hook #'my/flycheck-list-errors-only-when-errors)
    ;; (add-hook 'flycheck-mode-hook #'my/flycheck-list-errors-only-when-errors)
    ;; (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

    ;; use the npm version for the check
    ;; this breaks flycheck when we enter json-mode and perhaps others
    ;; an update seems to replace this anyways
    ;; (setq-default flycheck-disabled-checkers
    ;;               (append flycheck-disabled-checkers
    ;;                       '(javascript-jshint)))

    ;; use eslint with web-mode for jsx files
    ;; (flycheck-add-mode 'javascript-eslint 'web-mode)
    ;; (flycheck-add-mode 'javascript-jshint 'web-mode)
    )
  ;; (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change new-line))
  )

(defun my/flycheck-list-errors-only-when-errors ()
  "Show flycheck error list when there are errors in the current buffer."
  (defvar flycheck-current-errors)
  (defvar flycheck-error-list-buffer)
  (defvar-local buffer "")
  (message "checking for current errors")
  (if flycheck-current-errors
      (flycheck-list-errors)
    (-when-let (buffer (get-buffer flycheck-error-list-buffer))
      (dolist (window (get-buffer-window-list buffer))
        (quit-window nil window)))))

(defun dirty-init ()
  "A dump of init stuff found in dotspacemacs/user-config but is custom."

  ;; Works around this issue: https://github.com/syl20bnr/spacemacs/issues/9549
  (require 'helm-bookmark)

  (message "Loading user config")
  ;; debug
  ;; (setq-default tramp-verbose 6)
  ;; fixes tramp startup times
  (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

  ;; osx settings

  ;; web-mode
  (paradox-require 'web-mode)
  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (defvar web-mode-markup-indent-offset)
    (defvar web-mode-code-indent-offset)
    ;; why not setq-default?
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    )
  (add-hook 'web-mode-hook  'my-web-mode-hook)

  ;; indentation
  (paradox-require 'cc-mode)
  (defvar c-offsets-alist)
  (add-to-list 'c-offsets-alist '(arglist-close . c-lineup-close-paren))


  (load-library "config-company")
  (config-company)

  ;; non-nil indicates spacemacs should start with fullscreen
  (setq-default dotspacemacs-fullscreen-at-startup t)
  (defvar paradox-github-token)
  ;; actually this was dropped because we check this into github
  (setq paradox-github-token '663d5d3c21b2c6a716848fa00653bb92e6aeee68)
  (global-linum-mode) ; show line numbers by default
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)

  ;; prog-mode stuff
  ;; multi-line
  ;; always add new line rather than flowing like fci-mode
  (paradox-require 'multi-line)
  (defvar multi-line-always-newline)
  (setq-default multi-line-current-strategy
                (multi-line-strategy
                 :respace (multi-line-default-respacers
                           (make-instance multi-line-always-newline))))
  ;; (use-package "color-identifiers-mode"
  ;;   :ensure t
  ;;   :init
  ;;   (global-color-identifiers-mode)
  ;;   :config
  ;;   )

  ;; rainbow identifiers (aka semantic syntax highlighting)
  (use-package "rainbow-identifiers"
    :ensure t
    :init
    ;; (add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
    ;; (add-hook 'js2-mode-hook #'my/fix-js2-rainbow-identifiers)
    :config
    (setq-default rainbow-identifiers-faces-to-override
                  '(
                    ;; font-lock-type-face

                    ;; font-lock-variable-name-face
                    ;; font-lock-function-name-face
                    ;; js2-object-property
                    ;; js2-function-call
                    ;; js2-function-param
                    ;; js2-external-variable

                    ;; js2-object-property
                    ;; js2-instance-member
                    ;; js2-private-member
                    ;; js2-jsdoc-tag
                    ;; js2-jsdoc-value
                    ;; js2-jsdoc-type
                    ;; font-lock-constant-face
                    ;; font-lock-highlighting-faces

                    ))
    ;; (setq-default rainbow-identifiers-filter-functions
    ;;               (lambda (face)
    ;;                 (member face (list
    ;;                             "font-lock-comment-delimiter-face"
    ;;                             "font-lock-comment-face"
    ;;                             ))))
    :diminish 'rainbow-identifiers-mode
  )


  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."
  (paradox-require 'markdown-mode)
  (add-hook 'markdown-mode-hook 'auto-fill-mode)
  (add-hook 'markdown-mode-hook 'flyspell-mode)

  ;; graphviz dot support
  (package-initialize)
  (paradox-require 'graphviz-dot-mode)
  (setq-default graphviz-dot-preview-extension "png")
  (defun compile-dot ()
    "compile a graphviz dot file"
    ;; (compile graphviz-dot-dot-program))
    (defvar graphviz-dot-dot-program)
    (defvar graphviz-dot-preview-extension)
    (compile (concat graphviz-dot-dot-program
            " -T" graphviz-dot-preview-extension " "
            (shell-quote-argument buffer-file-name)
            " -o "
            (shell-quote-argument
             (concat (file-name-sans-extension buffer-file-name)
                     "." graphviz-dot-preview-extension))))
    )
  (add-hook 'graphviz-dot-mode-hook
            (lambda ()
             (add-hook 'after-save-hook 'compile-dot nil 'make-it-local)))

  ;; compilation
  ;; no need to show compile window on success - just interested in errors
  (defun compilation-exit-autoclose (STATUS code msg)
    "Close the compilation window if there was no error at all."
    ;; If M-x compile exists with a 0
    (when (and (eq STATUS 'exit) (zerop code))
      ;; then bury the *compilation* buffer, so that C-x b doesn't go there
      (bury-buffer)
      ;; and delete the *compilation* window
      (delete-window (get-buffer-window (get-buffer "*compilation*"))))
    ;; Always return the anticipated result of compilation-exit-message-function
    (cons msg code))
  (defvar compilation-exit-message-function)
  (setq compilation-exit-message-function 'compilation-exit-autoclose)

  ;; git gutter
  (setq-default git-gutter-fr+-side 'left-fringe)

  ;; fun!
  (paradox-require 'nyan-mode)
  (setq-default nyan-wavy-trail t)
  (setq-default nyan-animate-nyancat t)
  (setq-default nyan-animation-frame-interval 0.075)
  (setq-default nyan-bar-length 16)
  ;; as of spacemacs 0.200 this eats a ton of cpu time
  ;; (add-hook 'nyan-mode 'nyan-start-animation)
  ;; (add-hook 'change-major-mode-hook 'nyan-start-animation)

  ;; da faq?
  ;; animate letters inwards to the cursor point as you type
  ;; left for reference and not actual use - only works at top of file
  (defun animated-self-insert ()
    (let* ((undo-entry (car buffer-undo-list))
           (beginning (and (consp undo-entry) (car undo-entry)))
           (end (and (consp undo-entry) (cdr undo-entry)))
           (str (when (and (numberp beginning)
                           (numberp end))
                  (buffer-substring-no-properties beginning end)))
           (animate-n-steps 3))
      (when str
        (delete-region beginning end)
        (animate-string str (1- (line-number-at-pos)) (current-column)))))

  ;; (add-hook 'post-self-insert-hook 'animated-self-insert)

  (load-library "my-utils")
  (my/init-flycheck)
  (load-library "config-whitespace")
  (config-whitespace)
  (load-library "config-flyspell")
  (config-flyspell)
  (load-library "config-vc")
  (config-vc)
  (load-library "config-java")
  (config-java)
  (load-library "config-elm")
  (config-elm)
  (load-library "config-plantuml")
  (config-plantuml)
  (load-library "config-typescript")
  (config-typescript)
  (load-library "config-rainbow-mode")
  (config-rainbow-mode)
  ;; handle long lines
  (load-library "config-so-long-mode")
  (config-so-long-mode)
  (message "[EMACS-CONFIG] Configuring org-mode...")
  (load-library "config-org-mode")
  (config-org-mode)
  (message "[EMACS-CONFIG] org-mode configured...")


  (message "TODO: Find out how to use the current nvm version to find the bin dir for global node modules")
  (add-to-list 'exec-path "/Users/logan/.nvm/versions/node/v8.1.3/bin/")
  ;; TODO: move this into a general shell config file
  ;; Setting the shell to bash makes it work with things like exec-path. zsh
  ;; does not seem to work with this.
  (setq-default shell-file-name "bash")
  ;; (load-library "/Users/logan/dev/dotfiles/lisp/common-header-mode-line.pkg/common-header-mode-line.el")
  ;; (load-library "config-common-header-mode-line")
  ;; (config-common-header-mode-line)

  (load-library "org-to-jekyll")
  (load-library "renumber-list")
  (load-library "money")


  (setq-default grep-find-ignored-directories '(
                                               "tmp"
                                               ".tmp"
                                               ))

  (setq-default yas-snippet-dirs '("~/.emacs.d/private/snippets"))

  (message "[DIRTY INIT] INIT DONE!")
  )
(defun my/init ()
  "Do initializtion."
  (message "[INIT] Starting init.")
  (init-org-file "emacs-config.org")
  (dirty-init)
  (init-org-file "private.org")
  (init-org-file "buffer.org")
  (init-org-file "deft.org")
  (init-org-file "email.org")
  (init-org-file "whitespace.org")
  (init-org-file "habitica.org")
  (init-org-file "javascript.org")
  (init-org-file "groovy.org")
  (init-org-file "purescript.org")
  (init-org-file "css.org")
  (init-org-file "makefile.org")
  (init-org-file "hipchat.org")
  (init-org-file "tramp.org")
  (init-org-file "time.org")
  (init-org-file "diagram.org")
  (init-org-file "language-server-protocol.org")
  (init-org-file "java.org")
  (message "[INIT] Init Done.")
  )

(provide 'my/init)

;;; init.el ends here
