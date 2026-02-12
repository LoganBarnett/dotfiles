;;; editorconfig-config.el --- Configure editorconfig support -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Logan Barnett

;; Author: Logan Barnett <logustus@gmail.com>
;; Keywords: editorconfig

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

;; Read and apply .editorconfig settings for consistent project-wide
;; formatting (indentation, line endings, charset, trim trailing whitespace,
;; etc).

;;; Code:

(require 'use-package)

(use-package editorconfig
  :config
  ;; Enable editorconfig globally for all buffers.
  (editorconfig-mode 1)
  )

(provide 'editorconfig-config)
;;; editorconfig-config.el ends here
