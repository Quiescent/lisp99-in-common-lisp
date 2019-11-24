(defun lotto-select (n end)
  (if (eq n 0)
      nil
      (cons (random end)
            (lotto-select (1- n) end))))
