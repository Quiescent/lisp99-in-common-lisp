(defun split (xs count)
  (labels ((iter (ys first-segment i)
             (if (null ys)
                 nil
                 (if (eq i count)
                     (list first-segment (list ys))
                     (iter (cdr ys)
                           (nconc first-segment
                                  (list (car ys)))
                           (1+ i))))))
    (if (null xs)
        nil
        (iter xs nil 0))))
