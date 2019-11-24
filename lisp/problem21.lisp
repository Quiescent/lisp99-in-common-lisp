(defun insert-at (y xs index)
  (labels ((insert-at-iter (zs i)
             (cond
               ((null zs)    (error (format nil "Can't find index ~a to insert ~a at" i y)))
               ((eq i index) (cons y zs))
               (t            (cons (car zs)
                                   (insert-at-iter (cdr zs)
                                                   (1+ i)))))))
    (insert-at-iter xs 1)))
