;; Getting ChatGPT's API working involves a special premium subscription - one
;; independent from ChatGPT Plus.  This turned into too much work + money
;; promised until the sun burns out, so I'm shelving this work for now.
(defun openai-api-key-via-pass ()
  "Use `pass' to retrieve the API key for openai."
  (string-trim (shell-command-to-string "pass show openai-api-key"))
  )

(use-package org-ai
  :init
  ;; (setq request-backend 'url-retrieve)
  (setq org-ai-openai-api-token (openai-api-key-via-pass))
  )
