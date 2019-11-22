(defun my-flatten (xs)
  (cond
    ((null xs)        nil)
    ((consp (car xs)) (append (my-flatten (car xs))
                              (my-flatten (cdr xs))))
    (t                (cons (car xs)
                            (my-flatten (cdr xs))))))
