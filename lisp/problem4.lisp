(defun my-length (xs)
  (labels ((iter (ys length)
             (if (null ys)
                 length
                 (iter (cdr ys) (1+ length)))))
    (iter xs 0)))
