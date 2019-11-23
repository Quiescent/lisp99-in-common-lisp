(defun encode-direct (xs)
  (labels ((segment (z count)
             (if (eq count 1)
                 z
                 (cons count z)))
           (iter (ys z count)
             (cond
               ((null ys)       (list (segment z count)))
               ((eq (car ys) z) (iter (cdr ys) z (1+ count)))
               (t               (cons (segment z count)
                                      (iter (cdr ys) (car ys) 1))))))
    (if (null xs)
        nil
        (iter (cdr xs) (car xs) 1))))
