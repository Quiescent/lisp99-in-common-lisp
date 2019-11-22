(defun encode (xs)
  (let ((result))
    (nreverse
     (dolist (segment (pack xs) result)
       (push (cons (length segment)
                   (car segment))
             result)))))
