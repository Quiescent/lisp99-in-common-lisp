(defun is-palindrome (xs)
  (let ((reversed))
    (dolist (x xs)
      (push x reversed))
    (equal xs reversed)))
