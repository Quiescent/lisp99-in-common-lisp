(defun lsort (xss)
  (sort xss (lambda (ys zs) (< (length ys)
                          (length zs)))))
