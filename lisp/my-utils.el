;;; my-utils -- Various language level Lisp utilities
;;; Commentary:
;; A module providing Lisp utilities for handling generic data. This uses the
;; "my" prefix to avoid potential collisions with the very generic "utils" name.
;;; Code:

(defun my-utils/get-string-from-file (file-path)
  "Return FILE-PATH's file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

;; A function so we can debug better if need be.
(defun config/disable-visual-line-mode ()
  (visual-line-mode 0)
  )
;;; my-utils.el ends here
