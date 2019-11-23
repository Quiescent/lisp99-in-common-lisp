(defun rotate (xs count)
  (labels ((iter (ys acc i)
             (cond
               ((null ys)    nil)
               ((eq i count) (append ys acc))
               (t            (iter (cdr ys)
                                   (nconc acc
                                          (list (car ys)))
                                   (1+ i))))))
    (if (null xs)
        nil
        (iter xs nil 0))))
