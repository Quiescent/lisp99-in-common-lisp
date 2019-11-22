(defun my-reverse (xs)
  (let ((result))
    (dolist (x xs result)
      (push x result))))

(defun my-reverse-2 (xs)
  (labels ((iter (ys acc)
             (if (null ys)
                 acc
                 (iter (cdr ys) (cons (car ys) acc)))))
    (iter xs nil)))
