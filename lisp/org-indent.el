;;; org-indent.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Logan Barnett-Hoy
;;
;; Author: Logan Barnett-Hoy <http://github/logan>
;; Maintainer: Logan Barnett-Hoy <logustus@gmail.com>
;; Created: May 13, 2020
;; Modified: May 13, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/logan/org-indent
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Provide a fake org-indent.el so we don't load the real one.
;;
;;; Code:

(defun org-indent-mode (&rest _)
  (message "[CONFIG] Someone attempted to use org-indent-mode, but we stopped them!")
  nil
  )

(provide 'org-indent)
;;; org-indent.el ends here
