;;; python-config.el --- Python Emacs configuration.  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Logan Barnett

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

;;

;;; Code:
(use-package org
  :config
  (require 'ob-python)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (python . t)
     )
   )
  )


(provide 'python-config)
;;; python-config.el ends here
