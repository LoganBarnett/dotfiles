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

(package! auto-compile)
(package! dap-mode)
(package! docker)
(package! dockerfile-mode)
(package! ejira :recipe
  (:host github
   :repo "nyyManni/ejira"
   :files ("*.el")
   )
  ) ; Atlassian Jira integration.
(package! elm-mode)
(package! evil-iedit-state)
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
(package! glsl-mode)
;; We already have browse-at-remote, and I think browse-at-remote might be a
;; better library.
;;
;; (package! git-link)
(package! graphviz-dot-mode)
(package! groovy-mode)
(package! habitica)
(package! highlight-parentheses)
(package! lsp-java)
;; (package! latex-mode)
(package! lua-mode)
(package! js2-mode)
(package! lsp-mode)
(package! lsp-ui)
(package! markdown-mode)
;; mu4e's lisp build doesn't appear in a place that straight.el finds, so the
;; build directory for mu4e will appear as empty. To work around this we can
;; point it straight to the nix mu4e site-lisp directory. See
;; https://github.com/raxod502/straight.el/issues/491 for more context.
(package! mu4e
  :recipe (:local-repo "~/.nix-profile/share/emacs/site-lisp/mu4e"))
(package! multi-line)
(package! multi-term)
(package! noflet)
;; (package! org-contacts) ; Part of contrib, so this won't work.
(package! org-indent :disable t)
(package! org-mime)
(package! org-mode) ; Pretty sure some of these are built in.
(package! org-superstar :disable t)
(package! ox-gfm)
(package! pinentry)
(package! piper :recipe (:local-repo "~/dev/emacs-piper"))
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
(package! so-long)
;; I fail to see how this is ever helpful with vim bindings.
;; Though this attempt to disable it will not work according to
;; https://github.com/hlissner/doom-emacs/issues/1094
;; (package! smartparens :disable t)
;; (package! sql-mode)
(package! terraform-mode)
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
