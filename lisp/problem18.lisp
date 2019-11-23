(defun slice (xs start end)
  (labels ((iter (ys result i)
             (if (null ys)
                 result
                 (cond
                   ((and (>= i start) (<= i end)) (iter (cdr ys)
                                                        (nconc result (list (car ys)))
                                                        (1+ i)))
                   ((> i end)                     result)
                   (t                             (iter (cdr ys) result (1+ i)))))))
    (if (null xs)
        nil
        (iter xs nil 0))))
