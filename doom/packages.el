;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
;;; Code:

;; To install a package with Doom you must declare them here, run 'doom sync' on
;; the command line, then restart Emacs for the changes to take effect.
;; Alternatively, use M-x doom/reload.


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; For AppleScript files - automation for GUI applications in macOS.
;; Currently not supported by Doom's package manager.
;; (package! applescript-mode)
;; Lets me use Emacs to edit text from a browser form. Needs the Ghost Edit
;; Firefox plugin.
(package! atomic-chrome)
(package! auto-compile)
(package! dap-mode)
;; I like dash, but it's listed here because ejira or one of its dependencies
;; depends upon it, but has not declared that in its own package list.
;; dash-functional is marked as obsolete. I have tried updating ejira but it
;; seems like I'm current. Perhaps a pull request is in order.
(package! dash-functional)
(package! docker)
(package! dockerfile-mode)
(package! ejira :recipe
  (:host github
   :repo "nyyManni/ejira"
   :files ("*.el")
   )
  ) ; Atlassian Jira integration.
;; (package! ejira :recipe (:local-repo "~/dev/ejira"))
(package! elm-mode)
(package! enh-ruby-mode)
(package! evil-iedit-state)
(package! evil-nerd-commenter :pin "b8ac35fe019df5602c31912f65303a3d8ad0066c")
;; I could leave evil-snipe enabled to get highlighting from ; and ,.
;; See https://github.com/hlissner/doom-emacs/issues/1642
;; I decided to try it out, since the author is right - I can do the same things
;; with cl and cc.
;; (package! evil-snipe :disable t)
(package! feature-mode)
(package! find-lisp)
(package! floobits)
(package! flycheck)
(package! flyspell)
(package! flyspell-correct)
;; Include code blocks with language support in org->markdown exports.
(package! ox-gfm)
(package! glsl-mode)
;; We already have browse-at-remote, and I think browse-at-remote might be a
;; better library.
;;
;; (package! git-link)
(package! gnuplot)
(package! graphviz-dot-mode)
(package! groovy-mode)
(package! habitica)
(package! highlight-parentheses)
(package! keychain-environment)
(package! jiralib2 :recipe (:local-repo "~/dev/jiralib2"))
(package! lsp-java)
;; (package! latex-mode)
(package! lua-mode)
(package! js2-mode)
(package! lsp-mode)
(package! lsp-ui)
;; Desperately attempt to fix the SSH passphrase prompting issue.
(package! magit :pin "a4a78d341a7006ccdec708b424048ba3b22ee801")
(package! markdown-mode)
;; mu4e's lisp build doesn't appear in a place that straight.el finds, so the
;; build directory for mu4e will appear as empty. To work around this we can
;; point it straight to the nix mu4e site-lisp directory. See
;; https://github.com/raxod502/straight.el/issues/491 for more context.
(package! mu4e
  :recipe (
           :type built-in
           :local-repo "~/.nix-profile/share/emacs/site-lisp/mu4e"
           ;; pre-build must be disabled when using the nix derivation - in part
           ;; because it is alreadt built, but also because the directory is
           ;; read-only because that's how nix likes to do things.
           :pre-build ()
           )
  )
(package! multi-line)
(package! multi-term)
(package! noflet)
;; Org-babel support for OpenSCAD.
(package! ob-scad :recipe (:repo "https://github.com/wose/ob-scad.git"))
;; (package! org-contacts) ; Part of contrib, so this won't work.
(package! org-indent :disable t)
(package! org-mime)
;; Turn an org tree into a live, editable presentation!
(package! org-tree-slide)
;; (package! org-mode) ; Pretty sure some of these are built in.
;; Pinned due to https://github.com/doomemacs/doomemacs/issues/6478 (and
;; upstream, https://github.com/emacs-evil/evil/issues/1630 ).
(package! org-mode :pin "971eb6885ec996c923e955730df3bafbdc244e54")
(package! org-superstar :disable t)
;; Provides an org-mode export for a D&D LaTeX template.
;; https://github.com/evanbergeron/DND-5e-LaTeX-Template
(package! ox-dnd :recipe (:repo "https://github.com/xeals/emacs-org-dnd.git"))
;; ox-jira uses the trunk branch now, per
;; https://github.com/stig/ox-jira.el/issues/50, however straight.el hasn't
;; gotten detection of the default branch going yet (understandibly there's some
;; work involved, tracked at
;; https://github.com/raxod502/straight.el/issues/279). This should be all
;; that's needed to fix it.
(package! ox-jira :recipe (:branch "trunk"))
(package! ox-gfm)
;; Use the pass utility (https://www.passwordstore.org/).
(package! password-store)
;; Use pinentry to prompt for passwords via the gpg-agent.
(package! pinentry)
;; (package! piper :recipe (:local-repo "~/dev/emacs-piper"))
(package! piper :recipe
  (:host gitlab
   :repo "howardabrams/emacs-piper"
   :files ("*.el")
   )
  ) ; Atlassian Jira integration.
(package! plantuml-mode)
;; While perhaps a core package, this vexes me greatly. There are several ways
;; in which persp-mode offends my senses. One is that killing a buffer leaves it
;; in the buffer list. While this might be handy in some circumstances, part of
;; the reason I kill buffers is not only to free resources on my machine but
;; also remove clutter that my mind must sort through. This is especially
;; important with long tramp paths.
(package! persp-mode :disable t)
(package! prettier-js)
(package! puppet-mode)
(package! rainbow-identifiers)
(package! rainbow-delimiters)
(package! rainbow-mode)
(package! request-deferred)
(package! ruby-mode)
(package! rjsx-mode)
(package! rust-mode)
(package! scad-mode) ;; For opening OpenSCAD .scad files.
(package! scad-preview-mode :recipe (:repo "https://github.com/zk-phi/scad-preview.git"))
(package! so-long)
;; I fail to see how this is ever helpful with vim bindings.
;; Though this attempt to disable it will not work according to
;; https://github.com/hlissner/doom-emacs/issues/1094
;; (package! smartparens :disable t)
;; (package! sql-mode)
(package! terraform-mode)
(package! tree-sitter)
(package! tree-sitter-langs)
;; TypeScript - the highest mindshare typed JavaScript.
(package! typescript-mode)
;; Allow editing TypeScript with JSX.
;; (package! typescript-tsx-mode)
(package! web-beautify)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))
