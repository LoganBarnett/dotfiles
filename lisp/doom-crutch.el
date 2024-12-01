;;; package --- summary
;;; Commentary:

;; Copyright (C) 2024  Logan Barnett

;; Author: Logan Barnett <logan@scandium>
;; Keywords: doom

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

;; Bundle together all of the Doom stuff we still need into one place.  Most
;; files included are just blatant copies.

;;; Code:

(load-library "doom-plug")
;; This needs to come before the Doom keybindings, but I don't want to muck with
;; the original source file.  This can be bound up into a general "doom-theft"
;; module later.
(load-library "doom-constants")
;; Same as constants.
(load-library "doom-lib")
;; Same as constants.
(load-library "doom-use-package")
(load-library "doom-keybinds")
;; (load-library "doom-vertico.el")
;; (load-library "doom-projects.el")
;; (load-library "doom-popup-settings.el")

(provide 'doom-crutch)
;;; doom-crutch.el ends here
