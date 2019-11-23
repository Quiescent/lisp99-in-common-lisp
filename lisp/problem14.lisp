(defun dupli (xs)
  (if (null xs)
      nil
      (cons (car xs)
            (cons (car xs)
                  (dupli (cdr xs))))))
