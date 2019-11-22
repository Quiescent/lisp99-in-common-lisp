(defun my-last (xs)
  (cond
    ((null xs)       nil)
    ((null (cdr xs)) xs)
    (t               (my-last (cdr xs)))))
