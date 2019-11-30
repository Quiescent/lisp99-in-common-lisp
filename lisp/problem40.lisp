(defun goldbach (n)
  (let ((primes-below (remove nil (full-prime-table n))))
    (loop
      named outer
      with i = 0
      for prime across primes-below
      do (loop
           for other-prime across (subseq primes-below (1+ i))
           when (eq n (+ prime other-prime))
             do (return-from outer
                  (cons prime other-prime)))
      do (incf i))))
