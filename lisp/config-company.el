;;; config-company --- configure company-mode to my liking
;;; Commentary:
;; company mode handles auto completion

;;; Code:
(defun config-company ()
  "Run the company configuration."
  (defvar company-active-map)
  (defvar company-flow-modes)
  (declare-function paradox-require "ext:paradox-require")
  (declare-function global-company-mode "ext:global-company-mode")
  (declare-function evil-declare-change-repeat "ext:evil-declare-change-repeat")

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
  (with-eval-after-load 'company
    (message "configuring company after load")
    ;; keybindings
    (define-key company-active-map (kbd "RET") nil)
    (define-key company-active-map [12] nil)
    (define-key company-active-map [return] nil)
    (define-key company-active-map (kbd "TAB") 'company-complete-selection)
    (define-key company-active-map [tab] 'company-complete-selection)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)

    ;; backends
    (paradox-require 'company-flow)
    (add-hook 'company-mode-hook
              (apply-partially #'my/use-bin-from-node-modules "flow"))
    (spacemacs|defvar-company-backends javascript-mode)
    (add-to-list 'company-backends-javascript-mode 'company-flow)
    (add-to-list 'company-backends-js2-mode 'company-flow)
    )
  )

;; Commentary: add node_modules bin files for the project we are in
;; TODO: there is a very similar custom function for flychecker here
;; see about consolidating the two
;;; Code:
(defun my/use-bin-from-node-modules (bin-name)
  "Add node_modules bin files for project we are currently in.
BIN-NAME: the name of the binary to use"
  ;; (message "company -- setting %s exec for mode %s" bin-name major-mode )
  (defvar company-flow-executable)
  (let ((path "invalid")))
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (path (and root
                    (expand-file-name (concat "node_modules/.bin/" bin-name)
                                      root))))
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
