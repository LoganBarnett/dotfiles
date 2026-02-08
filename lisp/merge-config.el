;;; merge-config.el --- Declaratively configure settings in Emacs. -*- lexical-binding: t; -*-
;; It's kind of a pain to organize a non-trivial Emacs configuration.  Variables
;; get set from multiple locations and this can cause race conditions and the
;; like.  Instead of putting up with that, our merge-config.el package
;; accumulates settings and then sets them once.  While Emacs doesn't provide
;; any guarantees for this kind of thing, if one sticks to this in their local
;; config, they can ensure they can declaratively arrive at any value no matter
;; how many times the variable is supplied a value.
;;
;; This is largely inspired by how `nixpkgs' works within the Nix ecosystem.
;;; Commentary:
;; This is largely just a day dream and I haven't really started work in
;; earnest.  My plan is to look deeply about what `nixpkgs' is doing.  It uses
;; functions such as `mkMerge', `mkOverride', `mkOverride', `mkDefault',
;; `mkBefore', and `mkAfter' all to provide some control for setting a value.  I
;; think we can use something similar.  For phase 1, I planned on using
;; `use-package', though I understand there's some other package managers out
;; there that might enjoy this support.  I need to investigate `setopt' as a
;; contender for `setq' as well.
;;; Code:

(provide 'merge-config)
;;; merge-config.el ends here
