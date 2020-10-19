;;; path.el path utilities -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Logan Barnett-Hoy
;;
;; Author: Logan Barnett-Hoy <http://github/logan>
;; Maintainer: Logan Barnett-Hoy <logustus@gmail.com>
;; Created: October 18, 2020
;; Modified: October 18, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/logan/path
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(defun path/exec-find-on-exec-path (exec)
  "Find EXEC by walking `exec-path'."
  (concat
    (-find (lambda (p) (file-exists-p (concat p "/" exec))) exec-path)
    "/"
    exec
    )
  )

(provide 'path)
;;; path.el ends here
