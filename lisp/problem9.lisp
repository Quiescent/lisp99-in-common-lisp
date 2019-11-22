(defun pack (xs)
  (labels ((iter (ys x acc)
             (cond
               ((null ys)       nil)
               ((eq (car ys) x) (iter (cdr ys)
                                      x
                                      (cons x acc)))
               (t               (cons acc (iter (cdr ys) (car ys) (list (car ys))))))))
    (cdr (iter xs nil nil))))
