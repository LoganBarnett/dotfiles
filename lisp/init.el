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

(defun dirty-init ()
  "A dump of init stuff found in dotspacemacs/user-config but is custom."

  ;; Works around this issue: https://github.com/syl20bnr/spacemacs/issues/9549
  (require 'helm-bookmark)

  (message "Loading user config")
  ;; debug
  ;; (setq-default tramp-verbose 6)
  ;; fixes tramp startup times
  (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

  ;; Fixes issue where recentf runs into race conditions.
  ;; See https://github.com/syl20bnr/spacemacs/issues/5186 for more details.
  (cancel-timer recentf-auto-save-timer)

  ;; Purescript settings that drifted into the spacemacs config somehow.
  ;; (setq-default psc-ide-add-import-on-completion t t)
  ;; (setq-default psc-ide-rebuild-on-save nil t)

  ;; Allow these variables in .dir-locals.el
  (setq-default safe-local-variable-values (quote ((js-indent-level 4) (js2-indent-level . 4))))

  ;; indentation
  (paradox-require 'cc-mode)
  (defvar c-offsets-alist)
  (add-to-list 'c-offsets-alist '(arglist-close . c-lineup-close-paren))

  ;; non-nil indicates spacemacs should start with fullscreen
  (setq-default dotspacemacs-fullscreen-at-startup t)
  (defvar paradox-github-token)
  ;; actually this was dropped because we check this into github
  (setq paradox-github-token '663d5d3c21b2c6a716848fa00653bb92e6aeee68)
  ;; (global-linum-mode) ; show line numbers by default
  ;; Turn off line-number-mode so it doesn't overlap with
  ;; display-line-number-mode.
  (line-number-mode 0)
  ;; This calculates the current width of the line number column by doing an
  ;; initial count of the lines in the buffer. Without this, buffers with >=
  ;; 1000 lines will have an odd offset in them for lines >= the 1000 line
  ;; count.
  (setq-default display-line-numbers-width-start t)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)

  ;; prog-mode stuff
  ;; multi-line
  ;; always add new line rather than flowing
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

  ;; git gutter
  (setq-default git-gutter-fr+-side 'left-fringe)

  ;; fun!
  ;; (paradox-require 'nyan-mode)
  ;; (setq-default nyan-wavy-trail t)
  ;; (setq-default nyan-animate-nyancat t)
  ;; (setq-default nyan-animation-frame-interval 0.075)
  ;; (setq-default nyan-bar-length 16)

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

  ;; `elpa-mirror' provides a means of capturing the locally installed set of
  ;; packages in a form that can be consumed as an elpa mirror. This means we
  ;; can revert to a known working state in the case of a failed package
  ;; upgrade.
  (require 'elpa-mirror)
  (setq elpamr-default-output-directory (expand-file-name "~/dev/my-elpa"))
  ;; decide-mode comes from
  ;; https://github.com/lifelike/decide-mode/blob/master/decide.el
  (load-library "decide")
  (load-library "my-utils")
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
  ;; (load-library "config-typescript")
  ;; (config-typescript)
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

  (load-library "renumber-list")
  (load-library "money")


  (setq-default grep-find-ignored-directories '(
                                               "tmp"
                                               ".tmp"
                                               ))

  (message "[DIRTY INIT] INIT DONE!")
  )
(defun my/init ()
  "Do initializtion."
  ;; TODO: Move to macos.org when it gets merged.
  (set-frame-parameter nil 'fullscreen 'fullscreen)
  (load-library "redshift-indent")
  (message "[INIT] Starting init.")
  (auto-compile-on-load-mode 1)
  (init-org-file "emacs-config.org")
  (init-org-file "macos.org")
  (dirty-init)
  (init-org-file "evil.org")
  (init-org-file "flyspell.org")
  (init-org-file "messages.org")
  (init-org-file "flycheck.org")
  (init-org-file "company.org")
  (init-org-file "macos.org")
  (init-org-file "prog-mode.org")
  (init-org-file "json.org")
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
  (init-org-file "yasnippet.org")
  ;; (init-org-file "language-server-protocol.org")
  (init-org-file "java.org")
  (init-org-file "graphviz-dot.org")
  (init-org-file "markdown.org")
  (init-org-file "web.org")
  (init-org-file "font.org")
  (init-org-file "cucumber.org")
  (init-org-file "org-agenda.org")
  (message "[INIT] Init Done.")
  )

(provide 'my/init)

;;; init.el ends here
