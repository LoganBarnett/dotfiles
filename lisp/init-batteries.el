(defun batteries-init ()
  "Do initializtion."
  (message "[INIT] Starting init.")
  ;; Because Nix now copies this file where it needs to be, we need to add this
  ;; directory to the load path so my scattered .el files can be found.
  ;; (add-to-list 'load-path "~/dev/dotfiles/lisp/")
;;  (load-library "emacs-batteries")
  ;; (batteries-include
    ;; :bootstrap
    ;; Load the Doom theme.

    ;; Maximize on startup (but not fullscreen).
    (add-to-list 'default-frame-alist '(fullscreen . maximized))
    ;; Remove silly toolbar.
    (tool-bar-mode -1)
    ;; Remove the menubar (a macOS distinguishment from other UIs).
    (menu-bar-mode -1)
    ;; Remove scroll bars, which are useless to the initiated.
    (when scroll-bar-mode
      (scroll-bar-mode -1)
    )
    ;; Not sure where to put this, since it needs to apply globally.
    ;; Perhaps a global.org?
    (setq fill-column 80)
    (load-library "auto-compile")
    (setq-default load-prefer-newer t)
    (auto-compile-on-load-mode 1)
    ;; Themes must be loaded before macos, so we can add a hook and fix emoji
    ;; display.
    (init-org-file "theme.org")
    (load-library "scratch")
    (config/scratch-init)
    ;; Plug some holes in Doom because we're just taking files from it a la
    ;; carte.
    ;; Get our evil (vim) bindings working as soon as possible.
    (init-org-file "evil.org")
    (load-library "text-mode")
    (init-org-file "which-key.org")
    ;; Gives us custom-set-faces! and perhaps more.
    ;; (load-library "doom-lib-themes")
    (load-library "config-counsel")
    (load-library "dash")
    (setq-default load-prefer-newer t)
    (auto-compile-on-load-mode 1)
    ;; :editor
    ;; (init-org-file "org-mode.org")
    ;; :init

    ;; )
    (init-org-file "emacs-config.org")
    (init-org-file "macos.org")
    (init-org-file "gpg.org")
    (dirty-init)
    (init-org-file "debug.org")
    (init-org-file "prog-mode.org")
    (init-org-file "org-mode.org")
    (init-org-file "elisp-mode.org")
    ;; org-contacts adds the contacts file to org-agenda-files but this fails.
    ;; Some recent version of _something_ causes this to prompt to remove the
    ;; file from the list.  Since this happens during startup, naturally Emacs
    ;; just sits around with a white screen.  Disable org-contacts if the file
    ;; isn't present.  This might be a land mine later, but at least this will
    ;; prevent startup locks.
    (if (file-exists-p "~/notes/contacts.org")
      (init-org-file "org-contacts.org")
      nil
      )
    (init-org-file "keybindings.org")
    (init-org-file "modeline.org")
    (init-org-file "ui.org")
    (init-org-file "color.org")
    (init-org-file "printer2d.org")
    ;; This looks a little too much like clown barf right now. I need to fix it or
    ;; leave it off. For now it's disabled.
    ;; (init-org-file "rainbow-identifiers.org")
    (init-org-file "avy.org")
    (on-spacemacs (init-org-file "helm.org"))
    (init-org-file "flyspell.org")
    (init-org-file "messages.org")
    (init-org-file "flycheck.org")
    (init-org-file "company.org")
    ;; Themes must be loaded before macos, so we can add a hook and fix emoji
    ;; display.
    ;; (init-org-file "theme.org")
    (init-org-file "macos.org")
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
    (load-library "applescript-mode.el")
    (init-org-file "docker.org")
    (init-org-file "javascript.org")
    (init-org-file "groovy.org")
    (init-org-file "purescript.org")
    (init-org-file "scad.org")
    (init-org-file "svg.org")
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
    (init-org-file "keychain.org")
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
    (config/init-org-file-private "email-private.org")
    (config/init-org-file-private "jira-private.org")
    (config/init-org-file-private "org-agenda-private.org")
    (config/init-org-file-private "projectile-private.org")
    ;; Load up any ssh-agents or gpg-agents.
    (keychain-refresh-environment)
    (load-library "initial-buffer")
    (setq initial-buffer-choice #'config/initial-buffer)
    (message "[INIT] Done!")
  )

(provide 'batteries-init)