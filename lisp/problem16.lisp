(defun drop (xs n)
  (labels ((iter (ys i)
             (if (null ys)
                 nil
                 (if (eq 0 (mod i n))
                     (iter (cdr ys) 1)
                     (cons (car ys)
                           (iter (cdr ys) (1+ i)))))))
    (iter xs 1)))
