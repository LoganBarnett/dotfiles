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
  (interactive)
  (visual-line-mode -1)
  (global-visual-line-mode -1)
  )

;; Adapted from
;; https://emacs.stackexchange.com/questions/1051/copy-region-from-emacs-without-newlines
(defun config/yank-as-unfilled (beg end)
  "Save the current region to the kill ring after stripping extra whitespace and new lines"
  (interactive "r")
  (copy-region-as-kill beg end)
  (with-temp-buffer
    (yank)
    (goto-char 0)
    (while (looking-at "[ \t\n]")
      (delete-char 1))
    (compact-uncompact-block)
    (mark-whole-buffer)
    (kill-region (point-min) (point-max))))

(defun compact-uncompact-block ()
  "Remove or add line ending chars on current paragraph.
This command is similar to a toggle of `fill-paragraph'.
When there is a text selection, act on the region."
  (interactive)

  ;; This command symbol has a property “'stateIsCompact-p”.
  (let (currentStateIsCompact (bigFillColumnVal 4333999) (deactivate-mark nil))

    (save-excursion
      ;; Determine whether the text is currently compact.
      (setq currentStateIsCompact
            (if (eq last-command this-command)
                (get this-command 'stateIsCompact-p)
              (if (> (- (line-end-position) (line-beginning-position)) fill-column) t nil) ) )

      (if (region-active-p)
          (if currentStateIsCompact
              (fill-region (region-beginning) (region-end))
            (let ((fill-column bigFillColumnVal))
              (fill-region (region-beginning) (region-end))) )
        (if currentStateIsCompact
            (fill-paragraph nil)
          (let ((fill-column bigFillColumnVal))
            (fill-paragraph nil)) ) )

      (put this-command 'stateIsCompact-p (if currentStateIsCompact nil t)) ) ) )

(provide 'my-utils)
;;; my-utils.el ends here
