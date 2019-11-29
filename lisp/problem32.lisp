(defun my-gcd (x y)
  (if (eq 0 y)
      x
      (my-gcd y (mod x y))))
