(defun repli (xs count)
  (labels ((repeat (y)
             (loop for i from 0 below count collect y))
           (iter (ys)
             (if (null ys)
                 nil
                 (append (repeat (car ys))
                         (iter (cdr ys))))))
    (if (null xs)
        nil
        (iter xs))))
