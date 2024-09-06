(use-package counsel
  :ensure t
  :after general
  :config
  ;; Ivy typically has "^" for these values but I want nothing so we can have
  ;; fuzzy search from the start.
 (setq ivy-initial-inputs-alist
      '((org-refile . "")
        (org-agenda-refile . "")
        (org-capture-refile . "")
        (counsel-M-x . "")
        (counsel-describe-function . "")
        (counsel-describe-variable . "")
        (counsel-org-capture . "")
        (Man-completion-table . "")
        (woman . ""))) 
  (my-leader-def
    :keymaps 'normal
    "SPC" #'counsel-M-x
    )
  )
