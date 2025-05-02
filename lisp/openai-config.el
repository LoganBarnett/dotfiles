;;; openai-config.el --- My OpenAI / ChatGPT configuration.  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Logan Barnett

;; Author: Logan Barnett <logan@scandium>
;; Keywords:
;;; Description:
;;
;;; Code:

(defun openai-api-key-via-pass ()
  "Use `pass' to retrieve the API key for openai."
  (shell-command-to-string "pass show openai-api-key")
  )

(use-package chatgpt
  :init
  ;; (setq-default openai-key-auth-source #'openai-api-key-via-pass)
  (setq request-backend 'url-retrieve)
  (setq-default openai-key (openai-api-key-via-pass))
  :config
  (require 'openai)
  (require 'openai-chat)
  )


(provide 'openai-config)
;;; openai-config.el ends here
