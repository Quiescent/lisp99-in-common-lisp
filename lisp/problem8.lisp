(defun compress (xs)
  (cond
    ((null xs)               nil)
    ((null (cdr xs))         xs)
    ((eq (car xs) (cadr xs)) (compress (cons (car xs)
                                             (compress (cddr xs)))))
    (t                       (cons (car xs)
                                   (compress (cdr xs))))))
