;;; config-completion.el --- All of my completion configuration.  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Logan Barnett

;; Author: Logan Barnett <logan@scandium>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Completion doesn't come completely free in Emacs, and there are a lot of
;; options.  I've opted to try to follow Doom's completion choices, which
;; includes Vertico and its recommended, associated libraries.  That being said,
;; Doom has a lot of magic here, and this is where Doom makes some aggressive
;; assumptions.  Like many things in Doom, there's also things that seem like
;; they could easily be their own module but aren't, or things that appear to
;; belong in the upstream libraries, but aren't.  I also have no idea how it's
;; put together because it kind of is a big configuration that's been made
;; configurable (because of Doom's distribution status).  My configuration has
;; no such requirements, and I need to better understand these things so I will
;; build it myself.  Vertico's documentation seems to be rich with examples and
;; explanations.
;;
;; Near as I can tell, corfu and vertico are mutually exclusive to each other.
;; That's fine though.  It's probably better to have all completions in the same
;; spot.  I imagine Doom et al have done lots of work to separate them in the
;; right places, but I'd rather just take the easy/simple route for this one.
;; So far this includes the following completions I have noticed.  I have not
;; gone back through the documentation below to remove corfu, as I have decided
;; to remain with vertico.
;;
;; Spell checking, triggered via C-; :
;; 1. Dictionary/spelling suggestions.
;; Code completions, managed by corfu and triggered via TAB :
;; 1. Code completions (in `prog-mode').
;;
;; There are other things available I have not experimented with:
;; 1. corfu-echo - displays brief documentation on a selection candidate.
;; 2. corfu-info - access to location and documentation of candidates.
;; 3. cofru-popupinfo - Display documentation in a secondary pop-up.
;; 4. corfu-quick - Selection using Avy-style "quick keys", whatever those are.
;; 5. cape - Additional capf backends.  Example: file suggestions (`cape-file'),
;;    and dabbrev (`cape-dabbrev').
;;
;; Minibuffer completions, managed by vertico and triggered via TAB :
;; 1. I don't actually know if veritco is working here.
;;
;; If I wanted to consolidate these into the same place, I could use
;; `consult-completion-in-region'.  This is documented in the corfu README:
;; https://github.com/minad/corfu?tab=readme-ov-file#alternatives

;;; Code:

;; Enable vertico
(use-package! vertico
  ;; :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq-default
    read-file-name-completion-ignore-case t
    read-buffer-completion-ignore-case t
    completion-ignore-case t
    )
  )

(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  ;; (completion-styles '(basic substring partial-completion flex))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; (use-package corfu
;;   ;; Optional customizations
;;   ;; :custom
;;   ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
;;   ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
;;   ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
;;   ;; (corfu-preview-current nil)    ;; Disable current candidate preview
;;   ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
;;   ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches

;;   ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
;;   ;; :hook ((prog-mode . corfu-mode)
;;   ;;        (shell-mode . corfu-mode)
;;   ;;        (eshell-mode . corfu-mode))

;;   ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
;;   ;; be used globally (M-/).  See also the customization variable
;;   ;; `global-corfu-modes' to exclude certain modes.
;;   :init
;;   (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package nerd-icons-corfu
  :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  )

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package consult
  :init
  (setq completion-in-region-function 'consult-completion-in-region)
  )

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  )

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found.
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  )

(provide 'config-completion)
;;; config-completion.el ends here
