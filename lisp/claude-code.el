;;; claude-code.el --- Claude Code IDE               -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Logan Barnett

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

;; Configures claude-code-ide.el.

;; I'd hoped this would work as a great way to integrate vim/evil-mode bindings
;; into claude-code, but it doesn't work that great (I can't paste, for
;; example).  I think this is because the window in Emacs is just a direct `eat'
;; buffer to the Claude Code program.  I'd need to set vim mode in there, but it
;; has the additional problem of having a vim-in-vim experience that is
;; undesirable.  See https://github.com/manzaltu/claude-code-ide.el/issues/52
;; for troubleshooting others have done, as well as the problems I have
;; described herein.
;;
;; That said, there is a "vim" editing mode I can put it in, which renders all
;; of this rather redundant.  Still, I am keeping this around in case others
;; make good progress, or I decide to pick it back up again.

;;; Code:

(require 'use-package)

(use-package "claude-code-ide"
  :init
  (setq
   claude-code-ide-terminal-backend 'eat
   claude-code-ide-system-prompt nil
   claude-code-ide-cli-extra-flags ""
   )
  :config
  (map!
   :leader
   (:prefix
    ("C" . "claude-code")
    :desc "claude-code prompt" "c" #'claude-code-ide-menu
    :desc "claude-code resume" "R" #'claude-code-ide-resume
    :desc "claude-code continue" "C" #'claude-code-ide-continue
    :desc "claude-code debug menu" "D" #'claude-code-ide-debug-menu
    )
   )
  (claude-code-ide-emacs-tools-setup)
  )

(provide 'claude-code)
;;; claude-code.el ends here
