(defun config/initial-buffer ()
  (let ((buffer (get-buffer-create "*splash*")))
    (
      (evil-mode 1)
      (insert "foobar")
      (text-mode 1)
      buffer
      )
    )
  )
