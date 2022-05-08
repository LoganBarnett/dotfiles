;;; org-babel-execute-block.el --- Execute org-babel blocks  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Logan Barnett-Hoy
;;
;; Author: Logan Barnett-Hoy <logustus@gmail.com>
;; Maintainer: Logan Barnett-Hoy <logustus@gmail.com>
;; Created: May 07, 2022
;; Modified: May 07, 2022
;; Version: 0.0.1
;; Keywords: org org-babel
;; Homepage: https://github.com/loganbarnett/dotfiles
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Run a org-babel block by name.
;;
;;  Description
;;
;;; Code:

(defun org-babel-execute-block-by-name (blk-name)
  "Run an org-babel block by BLK-NAME."
  (save-excursion
    (org-babel-goto-named-src-block blk-name)
    (org-babel-execute-src-block)
    )
  )

(provide 'org-babel-execute-block)
;;; org-babel-execute-block.el ends here
