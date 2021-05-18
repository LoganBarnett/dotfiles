;;;  -*- lexical-binding: t; -*-
;;; init --- entry point for initializing the emacs config
;;; Commentary:
;; This is essentially the starting point for all of the Emacs config. Code can
;; go in .spacemacs, but that's harder to track since the file itself must
;; change over time during updates.

;;; Code:

;; Show messages while we start up.
;;(view-echo-area-messages)
(toggle-debug-on-quit)
;; Alternatively, this will show the entire messages buffer during startup.
;;
;; (with-current-buffer (messages-buffer)
;;   (goto-char (point-max))
;;   (switch-to-buffer (current-buffer)))

(defmacro on-doom (&rest body)
  "Execute BODY if this Emacs is running Doom Emacs."
  (if (boundp 'doom-version)
    `(progn ,@body)
    nil
    )
  )

(defmacro on-spacemacs (&rest body)
  "Execute BODY if this Emacs is running Spacemacs."
  (if (boundp 'spacemacs-version)
    `(progn ,@body)
    nil
    )
  )

(defun init-org-file (file)
  "Logs FILE before it loading a file to help with debugging init issues."
  (message "[INIT] %s" file)
  (org-babel-load-file (expand-file-name (format "org/%s" file) "~/dev/dotfiles"))
  )

(defun config/init-org-file-private (file)
  "Logs private FILE before it loading a file to help with debugging init issues."
  (message "[INIT] private %s" file)
  (org-babel-load-file
   (expand-file-name (format "org/%s" file) "~/dev/dotfiles-private")
   )
  )

(defun dirty-init ()
  "A dump of init stuff found in dotspacemacs/user-config but is custom."

  ;; An unintentional command-q (or H-q in Emacs jargon) will close all of
  ;; Emacs. Sometimes I get a nice warning that I can C-g out of when there's an
  ;; unsaved file, but if the GUI dialog comes up it seems like a shutdown is
  ;; all I have as an option. It is not clear if there is an option to abort the
  ;; shutdown. So we unbind the key. H-w might be next on the list, but that one
  ;; seems to be invoked less unintentionally.
  (global-unset-key (kbd "H-q"))

  ;; Works around this issue: https://github.com/syl20bnr/spacemacs/issues/9549
  (on-spacemacs (require 'helm-bookmark))

  (message "Loading user config")
  ;; debug
  ;; (setq-default tramp-verbose 6)
  ;; fixes tramp startup times
  (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

  ;; Fixes issue where recentf runs into race conditions.
  ;; See https://github.com/syl20bnr/spacemacs/issues/5186 for more details.
  (on-spacemacs (cancel-timer recentf-auto-save-timer))

  ;; Purescript settings that drifted into the spacemacs config somehow.
  ;; (setq-default psc-ide-add-import-on-completion t t)
  ;; (setq-default psc-ide-rebuild-on-save nil t)

  ;; Allow these variables in .dir-locals.el It is now removed because this
  ;; should be considered safe now, according to
  ;; https://github.com/mooz/js2-mode/issues/458#issuecomment-363208355 from
  ;; @eush77.
  ;;
  ;; Left for reference. This comes from "Don't know how to make a localized
  ;; variable an alias".
  ;;
  ;; (setq-default safe-local-variable-values (quote ((js-indent-level 4) (js2-indent-level . 4))))

  ;; indentation
  ;; (on-spacemacs (paradox-require 'cc-mode))
  (require 'cc-mode)
  (defvar c-offsets-alist)
  ;; This should prevent indentation rules from making argument lists or even
  ;; other lists from lining up with the last identifier, and instead use a more
  ;; scope-based approach. However I have not observed this indentation rule, as
  ;; I understand it, to be honored in all major-modes/languages, such as LISP,
  ;; HTML, and some others.
  (add-to-list 'c-offsets-alist '(arglist-close . c-lineup-close-paren))

  ;; non-nil indicates spacemacs should start with fullscreen
  (on-spacemacs (setq-default dotspacemacs-fullscreen-at-startup t))
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
  ;; (use-package "color-identifiers-mode"
  ;;   :ensure t
  ;;   :init
  ;;   (global-color-identifiers-mode)
  ;;   :config
  ;;   )

  ;; rainbow identifiers (aka semantic syntax highlighting)
  ;; (use-package "rainbow-identifiers"
  ;;   :ensure t
  ;;   :init
  ;;   ;; (add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
  ;;   ;; (add-hook 'js2-mode-hook #'my/fix-js2-rainbow-identifiers)
  ;;   :config
  ;;   (setq-default rainbow-identifiers-faces-to-override
                  ;; '(
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

                    ;; ))
    ;; (setq-default rainbow-identifiers-filter-functions
    ;;               (lambda (face)
    ;;                 (member face (list
    ;;                             "font-lock-comment-delimiter-face"
    ;;                             "font-lock-comment-face"
    ;;                             ))))
  ;;   :diminish 'rainbow-identifiers-mode
  ;; )

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

  ;; decide-mode comes from
  ;; https://github.com/lifelike/decide-mode/blob/master/decide.el
  (load-library "decide")
  (load-library "my-utils")
  (load-library "config-plantuml")
  (config-plantuml)
  ;; (load-library "config-typescript")
  ;; (config-typescript)
  ;; This is not rainbow identifiers but highlights color values as their
  ;; colors.
  (load-library "config-rainbow-mode")
  (config-rainbow-mode)
  ;; handle long lines
  (load-library "config-so-long-mode")
  (config-so-long-mode)

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

  (require 'highlight-parentheses)
  (global-highlight-parentheses-mode)
  (message "[DIRTY INIT] INIT DONE!")
  )
(defun my/init ()
  "Do initializtion."
  ;; (toggle-debug-on-error)
  ;; (setq-default projectile-indexing-method 'hybrid)
  ;; TODO: Move to macos.org when it gets merged.
  (set-frame-parameter nil 'fullscreen 'fullscreen)
  (load-library "redshift-indent")
  (message "[INIT] Starting init.")
  (setq-default load-prefer-newer t)
  (auto-compile-on-load-mode 1)
  (init-org-file "emacs-config.org")
  (init-org-file "macos.org")
  (dirty-init)
  (message "[INIT] Loading mu4e to fix startup issue...")
  ;; There is a startup error whose stack ends near org-store-link and within
  ;; that org-mu4e-store-link. org-mu4e-store-link is part of org-mu4e, which
  ;; shouldn't have been required yet, except for maybe the mu4e layer itself. I
  ;; still don't know why this adviced function is being invoked. For now just
  ;; require the file that makes it work.
  ;;
  ;; Historically, I've been able to remove the mu4e related elpa packages and
  ;; let Emacs reconstruct them on startup. I would have to do this on any
  ;; change to the email.org file, but other changes may have triggered the
  ;; issue as well.
  (require 'mu4e)
  (message "[INIT] mu4e load DONE")
  (init-org-file "debug.org")
  ;; (init-org-file "hydra.org")
  (init-org-file "org-mode.org")
  (init-org-file "org-contacts.org")
  (init-org-file "keybindings.org")
  (init-org-file "modeline.org")
  (init-org-file "ui.org")
  (init-org-file "color.org")
  ;; This looks a little too much like clown barf right now. I need to fix it or
  ;; leave it off. For now it's disabled.
  ;; (init-org-file "rainbow-identifiers.org")
  (init-org-file "evil.org")
  (init-org-file "avy.org")
  (on-spacemacs (init-org-file "helm.org"))
  (init-org-file "flyspell.org")
  (init-org-file "messages.org")
  (init-org-file "flycheck.org")
  (init-org-file "company.org")
  (init-org-file "macos.org")
  (init-org-file "prog-mode.org")
  (init-org-file "json.org")
  (init-org-file "conf-mode.org")
  (init-org-file "private.org")
  (init-org-file "buffer.org")
  ;; (init-org-file "deft.org")
  (init-org-file "whitespace.org")
  (init-org-file "habitica.org")
  (init-org-file "projectile.org")
  ;;
  ;; Begin languages. These should be sorted alphabetically.
  ;;
  (init-org-file "javascript.org")
  (init-org-file "groovy.org")
  (init-org-file "purescript.org")
  (init-org-file "scad.org")
  (init-org-file "css.org")
  (init-org-file "lisp.org")
  (init-org-file "makefile.org")
  (init-org-file "puppet.org")
  (init-org-file "ruby.org")
  (init-org-file "rust.org")
  (init-org-file "shell.org")
  (init-org-file "yaml.org")
  ;; End languages.
  (init-org-file "hipchat.org")
  (init-org-file "tramp.org")
  (init-org-file "time.org")
  (init-org-file "diagram.org")
  (init-org-file "yasnippet.org")
  (init-org-file "language-server-protocol.org")
  (init-org-file "java.org")
  (init-org-file "graphviz-dot.org")
  (init-org-file "html.org")
  (init-org-file "markdown.org")
  (init-org-file "web.org")
  (init-org-file "web-mode.org")
  (init-org-file "font.org")
  (init-org-file "piper.org")
  (init-org-file "cucumber.org")
  (init-org-file "git.org")
  (init-org-file "email.org")
  ;; org-agenda must be loaded after mu4e. The file itself does not call upon
  ;; mu4e directly, but perhaps something in org-agenda? This has been difficult
  ;; to track down. I might need to hook up some dependency hooks with
  ;; use-package to properly fix this.
  (init-org-file "org-agenda.org")
  (init-org-file "browser.org")
  (init-org-file "transportation-circle.org")
  (init-org-file "dnd.org")
  (init-org-file "jira.org")
  (init-org-file "theme.org")
  (config/init-org-file-private "jira-private.org")
  (config/init-org-file-private (concat
                                 ;; No one will ever figure this out!
                                 (s-join "" (reverse '("a" "e" "w" "n")))
                                 "-private.org"))
  (message "[INIT] Init Done.")
  )

(toggle-debug-on-quit)

(provide 'my/init)

;;; init.el ends here
