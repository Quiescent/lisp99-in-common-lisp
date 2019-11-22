(defun my-but-last (xs)
  (cond
    ((null xs)        nil)
    ((null (cdr xs))  nil)
    ((null (cddr xs)) xs)
    (t                (my-but-last (cdr xs)))))
