;;; org-babel-tangle-block.el --- Tangle org-babel blocks -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Logan Barnett-Hoy
;;
;; Author: Logan Barnett-Hoy <https://github.com/logan>
;; Maintainer: Logan Barnett-Hoy <logustus@gmail.com>
;; Created: November 06, 2021
;; Modified: November 06, 2021
;; Version: 0.0.1
;; Keywords: org org-babel
;; Homepage: https://github.com/loganbarnett/dotfiles
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Prefix arguments don't lend themselves to well-reasoned code. This gives us
;; the ability to tangle a code block by its name.
;;
;;; Code:

(require 'ob-tangle)

(defun org-babel-tangle-block (blk-name)
  "Tangle org-babel code block BLK-NAME."
  (save-excursion
    (let ((current-prefix-arg '(1)))
      (org-babel-goto-named-src-block blk-name)
      (org-babel-tangle)
      )
    )
  )

(provide 'org-babel-tangle)
;;; org-babel-tangle-block.el ends here
