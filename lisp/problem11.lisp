(defun encode-modified (xs)
  (if (null xs)
      nil
      (let ((encoded (encode xs)))
        (labels ((iter (ys)
                   (if (null ys)
                       nil
                       (cons (if (eq 1 (caar ys))
                                 (cdar ys)
                                 (car ys))
                             (iter (cdr ys))))))
          (iter encoded)))))
