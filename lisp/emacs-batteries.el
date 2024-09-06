
(defun batteries-include (&rest args)
  (dolist (e args)
    (if (symbolp e)
      nil
      (e)
      )
    )
  )

(provide 'batteries-include)
