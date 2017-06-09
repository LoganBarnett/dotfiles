;;; renumber-list --- rewrite the numbers in a list to match the order
;;; Commentary:
;; Rewrites all of the numbers in a list to match their current order. Can work
;; a region. Shamefully lifted from https://www.emacswiki.org/emacs/RenumberList

;;; Code:

;; renumber a list
(defun renumber-list (start end &optional num)
"Renumber the list items in the current START..END region.
If optional prefix arg NUM is given, start numbering from that
number instead of 1."
  (interactive "*r\np")
  (save-excursion
    (goto-char start)
    (setq num (or num 1))
    (save-match-data
      (while (re-search-forward "^[0-9]+" end t)
        (replace-match (number-to-string num))
        (setq num (1+ num))))))

(provide 'renumber-list)

;;; renumber-list.el ends here
