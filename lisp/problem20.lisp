(defun remove-at (xs pos)
  (labels ((iter (ys first-segment i)
             (cond
               ((null ys)  nil)
               ((eq i pos) (append first-segment (cdr ys)))
               (t          (iter (cdr ys)
                                 (nconc first-segment
                                        (list (car ys)))
                                 (1+ i))))))
    (if (null xs)
        nil
        (iter xs nil 1))))
