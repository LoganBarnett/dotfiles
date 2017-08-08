;;; config-email --- configure elm support
;;; Commentary:
;; Sets up email for use with gmail, Gnus, and gpg encryption.

;;; Code:

;; (require 'use-package)

(defun gmail-archive ()
  "Archive the current or marked mails.
This moves them into the All Mail folder."
  (interactive)
  (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/All Mail"))

(defun gmail-report-spam ()
  "Report the current or marked mails as spam.
This moves them into the Spam folder."
  (interactive)
  (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/Spam"))

(defun my/gnus-summary-keys ()
  (local-set-key (kbd "RET") 'gnus-summary-select-article)
  (local-set-key "C-w" 'evil-window-map)
  )

;; configure email
(defun config-email ()
  "Configure Email."
  (defvar-local this-file (or load-file-name buffer-file-name))
  (defvar-local this-dir (file-name-directory this-file))
  ;; No idea why setq-local is needed here, and elsewhere defvar-local works.
  ;; This would be a great question for the emacs user group.
  (setq-local key-id
           (my-utils/get-string-from-file
            (concat this-dir "key-id.txt")))
  (setq-default
   user-mail-address "logustus@gmail.com"
   mml-2015-signers key-id
   gnus-select-method
   '(nnimap "gmail"
            (nnimap-address "imap.gmail.com")
            (nnimap-server-port 993)
            (nnimap-stream ssl)
            )
   ;; u 41E46FB1ACEA3EF0 Logan Barnett (gpg key) <logustus@gmail.com>
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-smtp-service 587
   message-send-mail-function 'smtpmail-send-it
   nntp-authinfo-file "~/.authinfo.gpg"
   ;; Gmail system labels have the prefix [Gmail], which matches the default
   ;; value of gnus-ignored-newsgroups. That's why we redefine it.
   gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
   ;; The agent seems to confuse nnimap, therefore we'll disable it.
   gnus-agent nil
   ;; We don't want local, unencrypted copies of emails we write.
   gnus-message-archive-group nil
   ;; We want to be able to read the emails we wrote.
   mml2015-encrypt-to-self t
   )
  ;; Attempt to encrypt all the mails we'll be sending.
  (add-hook 'message-setup-hook 'mml-secure-message-encrypt)
  (add-hook 'gnus-summary-mode-hook 'my/gnus-summary-keys)

  ;; (use-package "email"
  ;; :init
  ;; :config
  ;; (setq-default email-indent-offset 2)
  ;; )
  )
(provide 'config-email)

;;; config-email.el ends here
