(defun decode (xs)
  (if (null xs)
      nil
      (append (destructuring-bind (x . xt) xs
                (cond
                  ((consp x) (loop for i from 0 below (car x) collect (cadr x)))
                  (t         (list x))))
              (decode (cdr xs)))))
