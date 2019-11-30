(defun full-prime-table (x)
  (let ((prime-table (make-array (list x))))
    (let ((i 0))
      (map-into prime-table (lambda (_) (setf (aref prime-table i) (1+ (incf i)))) prime-table ))
    (loop
      for i from 0 below (length prime-table)
      when (not (null (aref prime-table i)))
        do (loop
             for j from (+ i (aref prime-table i)) below (length prime-table) by (aref prime-table i)
             do (setf (aref prime-table j) nil))
      finally (return prime-table))))

(defun primes-from (start end)
  "Produce a list of prime numbers in range [START, END]."
  (remove-if (lambda (x) (or (null x)
                             (< x start)))
             (full-prime-table end)))
