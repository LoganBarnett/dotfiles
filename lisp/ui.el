;;; ui.el --- My UI settings for Emacs               -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Logan Barnett

;; Author: Logan Barnett <logan@scandium>
;; Keywords: lisp,
;;; Commentary:
;; This is just a bunch of UI settings to tune Emacs to my liking.  Much of it
;; was taken from here:
;; https://github.com/doomemacs/doomemacs/blob/master/lisp/doom-ui.el
;;; Code:

;;
;;; Windows/frames

;; Don't resize the frames in steps; it looks weird, especially in tiling window
;; managers, where it can leave unseemly gaps.
(setq frame-resize-pixelwise t)

;; But do not resize windows pixelwise, this can cause crashes in some cases
;; when resizing too many windows at once or rapidly.
(setq window-resize-pixelwise nil)

;; UX: GUIs are inconsistent across systems, desktop environments, and themes,
;;   and don't match the look of Emacs. They also impose inconsistent shortcut
;;   key paradigms. I'd rather Emacs be responsible for prompting.
(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
;; FIX: The native border "consumes" a pixel of the fringe on righter-most
;;   splits, `window-divider' does not. Available since Emacs 25.1.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
;; TODO: Use a real hook?
(add-hook 'doom-init-ui-hook #'window-divider-mode)

;; UX: Favor vertical splits over horizontal ones. Monitors are trending toward
;;   wide, rather than tall.
(setq split-width-threshold 160
      split-height-threshold nil)

;;
;;; Minibuffer

;; Allow for minibuffer-ception. Sometimes we need another minibuffer command
;; while we're in the minibuffer.
(setq enable-recursive-minibuffers t)

;; Show current key-sequence in minibuffer ala 'set showcmd' in vim. Any
;; feedback after typing is better UX than no feedback at all.
(setq echo-keystrokes 0.02)

;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(setq resize-mini-windows 'grow-only)

;; Typing yes/no is obnoxious when y/n will do
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  ;; DEPRECATED: Remove when we drop 27.x support
  (advice-add #'yes-or-no-p :override #'y-or-n-p))
;; HACK: By default, SPC = yes when `y-or-n-p' prompts you (and
;;   `y-or-n-p-use-read-key' is off). This seems too easy to hit by accident,
;;   especially with SPC as our default leader key.
(define-key y-or-n-p-map " " nil)

;; Try to keep the cursor out of the read-only portions of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; There is more to do, but I haven't had a chance to shop around yet.

(provide 'ui)
;;; ui.el ends here
