;;; config-d2-mode --- configure d2 support
;;; Commentary:
;;

;;; Code:

;; TODO: Reformat this into something a typical Lisp user would approve of,
;; and submit as a PR.
(defun org-babel-execute:d2 (body params)
  "Execute command with BODY and PARAMS from src block."
  (let* (
         (temp-file (org-babel-temp-file "d2-"))
         (file-header (cdr (assoc :file params)))
         (out-file (or file-header (format "%s.txt" temp-file)))
         (cmd (mapconcat #'shell-quote-argument
                         (if out-file
                           (append (list d2-location
                                       temp-file
                                       (org-babel-process-file-name out-file))
                                 d2-flags)
                           (append (list d2-location
                                       temp-file
                                       )
                                 d2-flags)
                           )
                           " "
                         )
              )
         )
         (with-temp-file temp-file (insert body))
         (org-babel-eval cmd "")
         ;; org-babel can output text if it is returned, which we will do if no
         ;; :file header was present.  This is especially useful for ascii
         ;; renderings.
         (if file-header
             nil
           (with-temp-buffer
             (insert-file-contents out-file)
             (buffer-string))
           )

         )
  )

(use-package d2-mode
  :config
  )

(provide 'config-d2-mode)
;;; config-d2-mode.el ends here
