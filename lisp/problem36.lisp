(defun prime-factors-mult (x)
  (let ((factors (prime-factors x)))
    (labels ((iter (xs y i acc)
               (cond
                 ((null xs)       (nreverse (cons (list y i) acc)))
                 ((eq (car xs) y) (iter (cdr xs) y        (1+ i) acc))
                 (t               (iter (cdr xs) (car xs) 1      (cons (list y i) acc))))))
      (iter (cdr factors)
            (car factors)
            1
            nil))))
