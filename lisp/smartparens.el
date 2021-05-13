;;; smartparens.el --- Summary -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Logan Barnett-Hoy
;;
;; Author: Logan Barnett-Hoy <https://github.com/logan>
;; Maintainer: Logan Barnett-Hoy <logustus@gmail.com>
;; Created: May 11, 2021
;; Modified: May 11, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/logan/smartparens
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Provide a fake smartparens.el so we don't load the real one.
;;
;;; Code:


(defun smartparens-mode (&rest _)
  "Fake it until you don't make it ever again."
  (message "[CONFIG] Someone attempted to use smartparens-mode, but we stopped them!")
  nil
  )

(provide 'smartparens)
;;; smartparens.el ends here
