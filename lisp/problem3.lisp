(defun element-at (xs i)
  (cond
    ((null xs) (error "List not big enough to find element at i"))
    ((eq i 1)  (car xs))
    (t         (element-at (cdr xs) (1- i)))))
