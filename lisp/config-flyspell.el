;;; config-flyspell --- configure flyspell-mode to my liking
;;; Commentary:
;; flyspell mode highlights spelling errors and can provide suggested
;; corrections

;;; Code:

;; configure flyspell
(defun config-flyspell ()
  (use-package "flyspell"
    :init
    (on-spacemacs (paradox-require 'flyspell-correct-helm))
    :config
    (define-key
      flyspell-mode-map
      (kbd "C-;")
      'flyspell-correct-previous-word-generic
      )
    )
    (message "set up flyspell-correct-helm - use C-; to invoke")
  )

  (provide 'config-flyspell)
;;; config-flyspell.el ends here
