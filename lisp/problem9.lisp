(defun pack (xs)
  (labels ((iter (ys x acc)
             (cond
               ((null ys)       (list acc))
               ((eq (car ys) x) (iter (cdr ys)
                                      x
                                      (cons x acc)))
               (t               (cons acc (iter (cdr ys) (car ys) (list (car ys))))))))
    (if (null xs)
        nil
        (cdr (iter xs nil nil)))))
