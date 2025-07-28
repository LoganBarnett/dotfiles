;;; eshell-config.el --- Configure eshell.           -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Logan Barnett

;; Author: Logan Barnett <logustus@gmail.com>
;; Keywords:

;;; Commentary:

;;; Code:

(eval-when-compile
  ;; for string-prefix-p and when-let
  (require 'subr-x)
  (require 'eshell)
  (require 'em-prompt)
  )

(declare-function eshell/pwd "em-prompt")
(declare-function eshell-search-path "em-dirs")
(declare-function process-lines "subr")

(defvar eshell-last-command-time nil)
(defvar eshell-last-command-duration nil)

(defun config/eshell-prompt-git-info ()
  "Compute git prompt info based if in a git repository."
  (when (eshell-search-path "git")
    (let (
          (branch (car (ignore-errors (process-lines "git" "rev-parse" "--abbrev-ref" "HEAD"))))
          (status (car (ignore-errors (process-lines "git" "status" "--porcelain"))))
          )
      (when branch
        (concat
         " on "
         (propertize branch 'face '(:foreground "magenta"))
         (when status
           (let (
                 (output (shell-command-to-string "git status --porcelain"))
                 )
             (let (
                   (added (length (seq-filter (lambda (l) (string-prefix-p "A " l))
                                              (split-string output "\n" t))))
                   (untracked (length (seq-filter (lambda (l) (string-prefix-p "??" l))
                                                  (split-string output "\n" t))))
                   )
               (concat "|"
                       (when (> added 0) (concat "✚" (number-to-string added)))
                       (when (> untracked 0) (concat "?" (number-to-string untracked)))
                       )
               )
             )
           )
         )
        )
      )
    )
  )

(defun config/eshell-prompt ()
  "Provide helpful prompt for eshell."
  ;; Record the time first so we can read it properly immediately afterwards.
  ;; `eshell-post-command-hook' fires after the prompt has been rendered - too
  ;; late for our purposes.
  (config/eshell-record-command-end-time)
  (let* ((path (propertize (abbreviate-file-name (eshell/pwd)) 'face '(:foreground "yellow")))
         (git-info (config/eshell-prompt-git-info))
         (user-host (propertize (concat (user-login-name) "@" (system-name)) 'face '(:foreground "cyan")))
         (status-code (propertize (number-to-string eshell-last-command-status)
                                  'face `(:foreground ,(if (= eshell-last-command-status 0) "green" "red"))))
         (timestamp (format-time-string "[%H:%M:%S]"))
         (duration (when (boundp 'eshell-last-command-duration)
                     (format "%ds" (truncate eshell-last-command-duration))))
         )
    (setq eshell-last-command-time (current-time))
    (concat
     path
     (or git-info "")
     " "
     user-host
     " "
     status-code
     " "
     timestamp
     " "
     duration
     "\n$ ")
    )
  )

(setq eshell-prompt-function #'config/eshell-prompt)

(defun config/eshell-record-command-start-time ()
  "Record start time of an eshell command to `eshell-last-command-time'."
  ;; (message "Recording start time as %s" (current-time))
  (setq eshell-last-command-time (current-time))
  )

(defun config/eshell-record-command-end-time ()
  "Using `eshell-last-command-duration', record duration of the command."
  (message "Recording duration...")
  (setq eshell-last-command-duration
        (float-time (time-subtract (current-time) eshell-last-command-time))
        )
  )

(add-hook 'eshell-pre-command-hook #'config/eshell-record-command-start-time)
;; While this doesn't trigger when we really want it to, we're leaving it in
;; because sometimes other things might trigger it when we want it to.  It would
;; seem that some things don't get triggered when submitting an empty prompt
;; (just pressing RET on a blank prompt), so other surprising machinery might be
;; in place too.  And in the current state with the hook present, all desired
;; behavior is intact.
(add-hook 'eshell-post-command-hook #'config/eshell-record-command-end-time)

;; The function is marked as obsolete but no alternative is given.
(with-no-warnings
  (setq eshell-prompt-regexp "^\\$ ")
 )

(provide 'eshell-config)
;;; eshell-config.el ends here
