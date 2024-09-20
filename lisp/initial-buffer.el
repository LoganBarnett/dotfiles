(defun config/initial-buffer ()
  (let ((buffer (get-buffer-create "*splash*")))
    (message "Evil-mode in initial buffer...")
    (text-mode)
    (evil-mode 1)
    (insert "foobar")
    buffer
    )
  )
