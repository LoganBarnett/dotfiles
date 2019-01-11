;;; config-org-mode --- configure org support
;;; Commentary:
;; Configure org-mode to my liking.
;;; Code:
(require 'use-package)
(require 'org)
(require 'org-mode-auto-id-headlines)

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

(defun config/hidden-content-indicator ()
  "Use something besides '...' to indicate hidden content in `org-mode'.

A common form of hidden content is collapsed headings."
  (setq-default org-ellipsis "⤵")
  )

(defun image-p (obj)
  "Return non-nil if OBJ is an image"
  (eq (car-safe obj) 'image))


;; I don't get why this doesn't seem to be logging, but it seems to be working.
(defun iimage-scale-to-fit-width ()
  "Scale over-sized images in the buffer to the width of the current window.
\(imagemagick must be enabled\)"
  (interactive)
  (let ((max-width (window-width (selected-window) t)))
    ;; (message "max-width %s" max-width)
    (org-element-map
        (org-element-parse-buffer 'object)
        'link
      (lambda (el)
        (let ((path (org-element-property :path el)))
          ;; (message "path %s" path)
          (when (string-match (image-file-name-regexp) path)
          ;; (when (image-p el)
          ;; ;; (message "el %s" el)
          ;; (when (equal "file" image)
            ;; (message "true")
            ;; (message "modifying el %s" el)
            ;; (message "width %s" (org-element-property :width el))
            (org-element-put-property el :type 'imagemagick)
            (org-element-put-property el :max-width max-width)
            (org-element-put-property el :width max-width)
            )
          )
        )
    ;; (let ((display (get-text-property (point-min) 'display)))
    ;;   (if (and (plist-member display 'max-width) (/= (plist-get display 'max-width) display))
          ;; (alter-text-property (org-element-property :begin el)
          ;;                      (org-element-property :end el)
          ;;                      'display
          ;;                      (lambda (prop)
          ;;                        (message "prop %s" prop)
          ;;                        (when (image-p prop)
          ;;                          (plist-put (cdr prop) :type 'imagemagick)
          ;;                          (plist-put (cdr prop) :max-width max-width)
          ;;                          ;; (plist-put (cdr prop) :width max-width)
          ;;                          ;; (plist-put (cdr prop) :scale t)
          ;;                          prop)
          ;;                      )
          ;; )))
      )
    )
    ;; )
  )

(defun iimage-scale-on-window-configuration-change ()
  "Hook function for major mode that display inline images:
Adapt image size via `iimage-scale-to-fit-width' when the window size changes."
  (add-hook 'window-configuration-change-hook #'iimage-scale-to-fit-width t t))

(defvar-local journal-file "/journal/.+\\.org")
(defun config/org-journal-file-p (path)
  "Return non-nil if PATH refers to a journal org-file."
  (string-match-p journal-file path)
  )
(defun config/org-not-journal-file-p (path)
  "Return non-nil if PATH refers _does not match_ a journal org-file."
  (not (config/org-journal-file-p path))
  )

;; Taken from https://emacs.stackexchange.com/a/12124/14851
(defun my/html2org-clipboard ()
  "Convert clipboard contents from HTML to Org and then paste (yank)."
  (interactive)
  (kill-new (shell-command-to-string "osascript -e 'the clipboard as \"HTML\"' | perl -ne 'print chr foreach unpack(\"C*\",pack(\"H*\",substr($_,11,-3)))' | pandoc -f html -t json | pandoc -f json -t org"))
  (yank))

;; configure org-mode
(defun config-org-mode ()
  "Configure 'org-mode'."
  ;; (package-initialize)
  (use-package "org"
  ;;   :requires (
  ;;              ;; Cover some languages we want supported.
  ;;              ob-js
  ;;              ob-sh
  ;;              ob-plantuml
  ;;              ;; Exporters.
  ;;              ox-confluence-en ;; Adds PlantUML support to Confluence exports.
  ;;              ox-gfm ;; Github Flavored Markdown.
  ;;              )
    :init
    :config
    ;; set default diary location
    (setq-default diary-file "~/notes/diary.org")
    ;; (setq-default appt-audible t)
    (setq-default calendar-date-style 'iso)

    (load-library "org-to-jekyll")
    ;; Having to manually specify these is a drag. Scanning all of ~/notes
    ;; sounds great on paper, but Emacs takes a long time to finish that scan. I
    ;; believe the ultimate solution is to condense my org files further into
    ;; larger files.
    (require 'find-lisp)
    (setq-default org-agenda-files
                  (seq-concatenate
                   'list
                   (find-lisp-find-files
                    (expand-file-name "~/Dropbox/notes")
                    "agenda.org")
                   '("~/work-notes/nwea.org")
                   )
                  )
    ;; shrink inline images see:
    ;; http://lists.gnu.org/archive/html/emacs-orgmode/2012-08/msg01388.html
    (setq-default org-src-fontify-natively t)
    ;; (setq-default org-image-actual-width '(564))
    ;; (setq-default org-image-actual-width nil)
    (add-hook 'org-mode-hook 'auto-fill-mode)
    ;; Use my custom org clock report function, which prevents narrowing. I find
    ;; narrowing during this operation confusing.
    ;; (add-hook 'org-mode-hook (lambda ()
    ;;                            (bind-key "C-c C-x C-r" 'my/org-clock-report)
    ;;                            ))
    (global-set-key (kbd "C-c C-x C-r") 'my/org-clock-report)
    ;; For some reason this doesn't work. How do I override key bindings?
    (bind-key (kbd "C-c C-x C-r") 'my/org-clock-report)

    (setq-default org-modules '(
                                ;; `org-checklist' clears checklists on tasks if
                                ;; `:RESET_CHECK_BOXES: t' is set for the
                                ;; properties on the task. I find this very
                                ;; useful for checklists in repeating tasks.
                                org-checklist
                                ))
    (require 'org-checklist)

    ;; Some initial langauges we want org-babel to support
    (require 'ob-js)
    (require 'ob-shell)
    (require 'ob-plantuml)
    ;; Exporters.
    (require 'ox-confluence-en) ;; This one adds PlantUML support.
    (require 'ox-gfm) ;; Github Flavored Markdown.
    (org-babel-do-load-languages
     'org-babel-load-languages
     '(
       (ditaa . t)
       (dot . t)
       (emacs-lisp . t)
       (gnuplot . t)
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
    ;; (setq-default imagemagick-enabled-types t)
    ;; imagemagick-register-types must be invoked after changing enabled types.
    (imagemagick-register-types)

    ;; Solution lifted from https://emacs.stackexchange.com/a/33963
    ;; Somehow this doesn't appear to be working for jpegs of large width. They
    ;; get clipped, which is undesirable.
    (add-hook 'org-mode-hook #'iimage-scale-on-window-configuration-change)

    (use-friendly-deterministic-headline-html-anchors)
    (config/hidden-content-indicator)
    )
  )
(provide 'config-org-mode)

;;; config-org-mode.el ends here
