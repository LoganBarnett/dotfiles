;;; config-company --- configure company-mode to my liking
;;; Commentary: company mode handles auto completion

;;; Code:
(defun config-company ()
  "Run the company configuration."

  (paradox-require 'company)
  ;; company-mode (for auto-complete)
  (global-company-mode 1)
  ;; fast auto-complete
  (setq-default company-idle-delay 0.2)
  (setq-default company-minimum-prefix-length 1)
  (global-set-key (quote [(ctrl return)]) 'company-complete)
  (setq-default company-auto-complete t)
  ;; (define-key company-active-map [tab] 'company-select-next)
  (setq-default company-auto-complete-chars [41 46])
  ;; keep evil mode and company mode from conflicting
  ;; see https://github.com/company-mode/company-mode/issues/383
  (evil-declare-change-repeat 'company-complete)
  ;; (with-eval-after-load 'company
    (message "configuring company after load")
    ;; keybindings
    (define-key company-active-map (kbd "RET") nil)
    (define-key company-active-map [12] nil)
    (define-key company-active-map [return] nil)
    (define-key company-active-map (kbd "TAB") 'company-complete-selection)
    (define-key company-active-map [tab] 'company-complete-selection)

    ;; backends
    (paradox-require 'company-flow)
    '(add-to-list 'company-backends 'company-flow)
    (add-to-list 'company-flow-modes 'javascript-mode)
    (message "-------- do we get this far?")
    (add-hook 'company-mode-hook
              (apply-partially #'my/use-bin-from-node-modules "flow"))
    (message "company -- done with configuration")
    ;; )
  )

;; Commentary: add node_modules bin files for the project we are in
;; TODO: there is a very similar custom function for flychecker here
;; see about consolidating the two
;;; Code:
(defun my/use-bin-from-node-modules (bin-name)
  ;; (message "company -- setting %s exec for mode %s" bin-name major-mode )
  (setq path "invalid")
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (path (and root
                    (expand-file-name (concat "node_modules/.bin/" bin-name)
                                      root))))
    ;; (message "company -- path is %s" path)
    (if path
        ;; (when (file-executable-p path)
        (let ((exec-sym (intern (concat "company-" bin-name "-executable"))))
          (make-local-variable exec-sym)
          (set exec-sym path)
          (message "company -- exec is %s" company-flow-executable)
          )
      ;; (message "company -- backend %s not available for mode %s with file %s"
               ;; bin-name major-mode buffer-file-name)
      )
    )
  )

(provide 'config-company)
;;; config-company.el ends here
