(defun totient-phi (x)
  (loop
    for i from 0 below x
    count (coprime x i)))
