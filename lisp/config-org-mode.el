;;; config-org-mode --- configure org support
;;; Commentary:
;; Configure org-mode to my liking.
;;; Code:
(require 'use-package)
(require 'org)

;; TODO: Setup a keybinding to replace org-clock-report with this function.
(defun my/org-clock-report ()
  "Run org-clock-report but don't leave a narrowed buffer when done."
  (interactive)
  (org-clock-report)
  (widen))

;; Lifted from
;; https://emacs.stackexchange.com/questions/21124/execute-org-mode-source-blocks-without-security-confirmation
(defun my/org-confirm-babel-evaluate (lang body)
  "Prevents evaluation of LANG if it is in the list below. BODY is not used."
  (not (member lang '("plantuml"))))

;; configure org-mode
(defun config-org-mode ()
  "Configure 'org-mode'."
  (package-initialize)
  ;; (use-package "org-mode"
    ;; :init
    ;; :config
    ;; set default diary location
    (setq-default diary-file "~/notes/diary.org")
    ;; (setq-default appt-audible t)
    (setq-default calendar-date-style 'iso)
    (setq-default org-agenda-files '("~/notes/planner.org"))
    ;; shrink inline images see:
    ;; http://lists.gnu.org/archive/html/emacs-orgmode/2012-08/msg01388.html
    (setq-default org-src-fontify-natively t)
    ;; (setq-default org-image-actual-width '(400))
    (setq-default org-image-actual-width nil)
    (add-hook 'org-mode-hook 'auto-fill-mode)
    ;; Use my custom org clock report function, which prevents narrowing. I find
    ;; narrowing during this operation confusing.
    ;; (add-hook 'org-mode-hook (lambda ()
    ;;                            (bind-key "C-c C-x C-r" 'my/org-clock-report)
    ;;                            ))
    (global-set-key (kbd "C-c C-x C-r") 'my/org-clock-report)
    ;; For some reason this doesn't work. How do I override key bindings?
    (bind-key (kbd "C-c C-x C-r") 'my/org-clock-report)
    ;; Some initial langauges we want org-babel to support
    (require 'ob-js)
    (require 'ob-sh)
    (require 'ob-plantuml)
    ;; Exporters.
    (require 'ox-confluence-en) ;; This one adds PlantUML support.
    (org-babel-do-load-languages
     'org-babel-load-languages
     '(
       (ditaa . t)
       (dot . t)
       (emacs-lisp . t)
       (js . t)
       (octave . t)
       ;; (perl . t)
       (plantuml . t)
       ;; (python . t)
       ;; (ruby . t)
       (shell . t)
       ;; (sqlite . t)
       ;; (R . t)
       ))
    (setq-default org-confirm-babel-evaluate 'my/org-confirm-babel-evaluate)
    )
;; )
(provide 'config-org-mode)

;;; config-org-mode.el ends here
