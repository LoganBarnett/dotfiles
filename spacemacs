;; -*- mode: dotspacemacs -*-
;; -*- mode: emacs-lisp -*-
;;; spacemacs --- spacemacs configuration
;;; Commentary:
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.
;;; Code:

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;; (shell :variables
     ;; better-defaults
     ;; org
     ;; this breaks tests?
     ;; psc-ide
     auto-completion
     docker
     elm
     emacs-lisp
     git
     helm
     html
     java
     javascript
     markdown
     org
     osx
     purescript
     spell-checking
     syntax-checking
     typescript
     version-control
     vimscript
     yaml
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
   '(
     ;; color-identifiers-mode
     ;; company-mode
     ;; flycheck-css-color
     ;; flycheck-json
     ;; hopefully managed by a spacemacs layer
     company-flow
     floobits
     flycheck
     flycheck-flow
     flycheck-purescript
     graphviz-dot-mode
     multi-line
     multi-term
     nyan-mode
     plantuml-mode
     prettier-js
     rainbow-identifiers
     rainbow-mode
     web-beautify
     )
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages nil))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update nil
   ;; dotspacemacs-check-for-update t
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'random
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light
                         solarized-light
                         solarized-dark
                         leuven
                         monokai
                         zenburn)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               ;; :size 13
                               :size 11
                               :weight normal
                               :width normal
                               :powerline-scale 1)
                               ;; :powerline-scale 1.1)
                               ;; :powerline-scale 1.5)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; (Not implemented) dotspacemacs-distinguish-gui-ret nil
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ t
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text t
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts t
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup t
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers t
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ;; HACK: workaround for https://github.com/syl20bnr/spacemacs/issues/8091
   ;; dotspacemacs-helm-use-fuzzy 'source
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
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
    ;; (add-hook 'flycheck-mode-hook #'flycheck-purescript-setup)
    ;; (add-hook 'prog-mode #'flycheck-mode)
    ;; (global-flycheck-mode)
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
    (add-hook 'flycheck-mode-hook (apply-partially #'my/use-checker-from-node-modules "flow"))
    (add-hook 'flycheck-mode-hook (apply-partially #'my/use-checker-from-node-modules "eslint"))
    (add-hook 'flycheck-mode-hook (apply-partially #'my/use-checker-from-node-modules "jshint"))

    ;; use the npm version for the check
    ;; this breaks flycheck when we enter json-mode and perhaps others
    ;; an update seems to replace this anyways
    ;; (setq-default flycheck-disabled-checkers
    ;;               (append flycheck-disabled-checkers
    ;;                       '(javascript-jshint)))

    ;; use eslint with web-mode for jsx files
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (flycheck-add-mode 'javascript-jshint 'web-mode)


    ;; purescript
    ;; (setq-default flycheck-purescript-compile-output-dir "output")
    (paradox-require 'flycheck-flow)
    (use-package "flycheck-flow"
      :init
      (flycheck-add-mode 'javascript-flow 'js-mode)
      (flycheck-add-mode 'javascript-flow 'web-mode)
      :config
      ;; (add-hook 'flycheck-mode-hook #'my/use-flow-from-node-modules)
      ;; only check flow annotated files with flow
      (setq-default flycheck-javascript-flow-args '("--respect-pragma"))

      (message "added flow to flycheck")
      )
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

;; shameless grab from
;; http://rejeep.github.io/emacs/elisp/2010/11/16/delete-file-and-buffer-in-emacs.html
(defun my/delete-file-and-kill-buffer ()
    "Remove file connected to current buffer and kill buffer."
    (interactive)
    (let ((filename (buffer-file-name))
          (buffer (current-buffer))
          (name (buffer-name)))
      (if (not (and filename (file-exists-p filename)))
          (error "Buffer '%s' is not visiting a file!" name)
        (when (yes-or-no-p "Are you sure you want to remove this file? ")
          (delete-file filename)
          (kill-buffer buffer)
          (message "File '%s' successfully removed" filename)))))

(defun dotspacemacs/user-config ()
  "This is a safe place to stick user-specific configuration for Spacemacs."
  (message "Loading user config")
  ;; add load-path for packages not in the melpa database
  (add-to-list 'load-path "~/.emacs.d/private/local/dotfiles")
  ;; get tern on the path
  ;; TODO: Make this home dir agnostic - maybe just use which
  ;; (add-to-list 'load-path "/Users/logan/.nvm/versions/node/v5.11.1/bin")

  ;; debug
  ;; (setq-default tramp-verbose 6)
  ;; fixes tramp startup times
  (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

  ;; encryption
  (require 'epa-file)
  (epa-file-enable)
  ;; (paradox-require 'pinentry)
  (defvar epa-pinentry-mode)
  (setf epa-pinentry-mode 'loopback)


  ;; turn off the menu bar so we can see things like the time on small screens
  (menu-bar-mode -1)

  ;; modeline settings
  ;; (paradox-require 'diminish nil t)
  ;; (setq-default diminish-mode-alist '(company-mode))
  ;; (eval-after-load "company-mode" '(diminish 'company-mode 'auto-complete-mode))
  ;; (eval-after-load 'yas-minor-mode '(diminish 'yas-minor-mode nil))
  ;; (with-eval-after-load 'company-mode
  ;;   (spacemacs|diminish 'company-mode nil))
  ;; (spacemacs|diminish 'yas-minor-mode nil)
  ;; (spacemacs|diminish 'flyspell-mode nil)
  ;; (spacemacs|diminish 'which-key-mode nil)
  ;; (spacemacs|diminish 'smartparens-mode nil)
  ;; (require 'delight)
  ;; (delight 'yas-minor-mode)
  ;; (require 'delight-powerline)
  ;; (when (paradox-require 'dim nil t)
  ;;                    (dim-minor-names '((
  ;;                                        yas-minor-mode "foo"
  ;;                                        company-mode ""
  ;;                                        flycheck-mode ""
  ;;                                        flyspell-mode "")))
  ;;                    )

  ;; turning off individual lighters doesn't work using the "blessed"
  ;; spacemacs|diminish function (see commented code above)
  ;; so just turn the damn thing off entirely
  (spaceline-toggle-minor-modes-off)
  ;; (setq-default spaceline-separator-dir-left '(left . left))
  ;; (setq-default spaceline-separator-dir-right '(right . right))
  (setq-default powerline-default-separator nil)
  (spaceline-compile)

  ;; osx settings
  (setq-default mac-command-key-is-meta t)
  (setq-default mac-option-modifier 'alt)
  (setq-default osx-use-option-as-meta nil)
  (setq-default mac-option-key-is-meta nil)
  (setq-default mac-command-modifier 'meta)
  ;; set keys for Apple keyboard, for emacs in OS X
  ;; for other OSes and reference, see
  ;; http://ergoemacs.org/emacs/emacs_hyper_super_keys.html
  (setq-default mac-command-modifier 'meta) ; make cmd key do Meta
  ;; (setq mac-option-modifier 'super) ; make opt key do Super
  (setq-default mac-right-command-modifier 'super)
  ;; (setq mac-control-modifier 'control) ; make Control key do Control
  (setq-default ns-function-modifier 'hyper)  ; make Fn key do Hyper
  ;; not an osx setting, but keyboard related (maybe move all of these to
  ;; keyboard section)
  ;; (define-key 'key-translation-map (kbd "<menu>") 'super)
  ;; (setq-default w32-apps-modifier 'super)
  ;; (global-unset-key (kbd "<menu>"))
  ;; TODO: this doesn't seem to do anything useful
  ;; (define-key key-translation-map (kbd "C-p") (kbd "<menu>"))
  ;; (global-set-key (kbd "<menu>") (lambda () (interactive) 'super))
  (setq-default ns-right-alternate-modifier 'super)
  ;; (define-key key-translation-map (kbd "C-p") 'super)
  ;; (key-chord-define-global "<menu>" (lambda () (interactive) 'super))

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

  ;; css-mode

  ;; indentation
  (setq-default css-indent-offset 2)
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

  ;; highlight lines longer than 80 chars
  ;; (require 'whitespace)
  ;; (setq whitespace-style '(tabs face empty lines-tail trailing))
  ;; (global-whitespace-mode t)
  ;; taken from https://www.emacswiki.org/emacs/EightyColumnRule
  ;; (add-hook 'font-lock-mode-hook #'my/highlight-gt-80-columns)
  (add-hook 'prog-mode-hook #'my/highlight-gt-80-columns)
  (add-hook 'text-mode-hook #'my/highlight-gt-80-columns)

  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."
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


  ;; (add-hook 'buffer-list-update-hook 'turn-on-fci-mode)
  (paradox-require 'markdown-mode)
  (add-hook 'markdown-mode-hook 'auto-fill-mode)
  (add-hook 'markdown-mode-hook 'flyspell-mode)
  ;; (add-hook 'markdown-mode-hook (lambda () (auto-fill-mode)))
 ;;(setq-default fci-rule-width 1)
  ;;(setq-default fci-rule-color "darkblue")

  ;; purescript-mode
  (setq-default psc-ide-client-executable "/usr/local/bin/psc-ide-client")
  (setq-default psc-ide-server-executable "/usr/local/bin/psc-ide-server")
  (setq-default psc-ide-rebuild-on-save t)
  (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)

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

  (my/init-flycheck)
  (load-library "config-flyspell")
  (config-flyspell)
  (load-library "config-vc")
  (config-vc)
  (load-library "config-java")
  (config-java)
  (load-library "config-elm")
  (config-elm)
  (load-library "config-javascript")
  (config-javascript)
  (load-library "config-plantuml")
  (config-plantuml)
  (load-library "config-typescript")
  (config-typescript)
  (load-library "config-rainbow-mode")
  (config-rainbow-mode)
  ;; handle long lines
  (load-library "config-so-long-mode")
  (config-so-long-mode)
  (load-library "config-org-mode")
  (config-org-mode)

  (load-library "org-to-jekyll")
  (load-library "renumber-list")
  (load-library "money")


  (setq-default grep-find-ignored-directories '(
                                               "tmp"
                                               ".tmp"
                                               ))

  ;; (if (locate-library "flycheck-purescript-mode")
  ;;     (autoload "flycheck-purescript-mode" "flycheck-purescript-mode" "checker for purescript" t)
  ;;   ;; TODO: make this a mode hook for purescript-mode
  ;;   (eval-after-load 'flycheck
  ;;     '(flycheck-purescript-setup))
  ;;   )

  ;; old flycheck settings - didn't work
  ;; (paradox-require 'flycheck)
  ;; disable jshint since we prefer eslint checking
;;  (setq-default flycheck-disabled-checkers
;;    (append flycheck-disabled-checkers
;;      '(javascript-jshint)))
  ;; (eval-after-load
  ;;     'flycheck
  ;;   (lambda ()
  ;;     (flycheck-add-mode 'javascript-eslint 'js2-mode)
  ;;     ;; Disable jshint
  ;;     ;; wait, why are we doing this?
  ;;     (setq-default
  ;;      flycheck-disabled-checkers
  ;;      (append flycheck-disabled-checkers
  ;; 	     '(javascript-jshint)))))

)

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol t)
 '(flycheck-javascript-flow-args nil)
 '(package-selected-packages
   (quote
    (plantuml-mode prettier-js dockerfile-mode docker tablist docker-tramp tide typescript-mode org-projectile org-present org-pomodoro alert log4e gntp org-download htmlize gnuplot floobits multi-term diminish s rainbow-mode winum fuzzy f smeargle orgit org magit-gitflow helm-gitignore gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link evil-magit magit magit-popup packed auto-complete evil flyspell-correct async multiple-cursors avy simple-httpd haml-mode dash multi-line shut-up company-emacs-eclim eclim iedit markdown-mode bind-key tern smartparens bind-map highlight flycheck git-commit with-editor company projectile helm helm-core yasnippet skewer-mode js2-mode hydra purescript-mode vimrc-mode dactyl-mode rainbow-identifiers color-identifiers-mode color-identifiers define-word yaml-mode ws-butler window-numbering which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toc-org tagedit spacemacs-theme spaceline slim-mode scss-mode sass-mode reveal-in-osx-finder restart-emacs request rainbow-delimiters quelpa pug-mode psci psc-ide popwin persp-mode pcre2el pbcopy paradox osx-trash osx-dictionary org-plus-contrib org-bullets open-junk-file nyan-mode neotree move-text mmm-mode markdown-toc macrostep lorem-ipsum livid-mode linum-relative link-hint less-css-mode launchctl json-mode js2-refactor js-doc info+ indent-guide ido-vertical-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag graphviz-dot-mode google-translate golden-ratio git-gutter-fringe git-gutter-fringe+ gh-md flyspell-correct-helm flycheck-purescript flycheck-pos-tip flycheck-flow flycheck-elm flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu emmet-mode elm-mode elisp-slime-nav dumb-jump diff-hl company-web company-tern company-statistics company-flow column-enforce-mode coffee-mode clean-aindent-mode auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(psc-ide-add-import-on-completion t t)
 '(psc-ide-rebuild-on-save nil t)
 '(safe-local-variable-values (quote ((js2-indent-level . 4)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; spacemacs ends here
