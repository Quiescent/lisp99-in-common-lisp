(defun prime-table (x)
  (let ((prime-table (make-array (list (1- (floor x 2))))))
    (let ((i 0))
      (map-into prime-table (lambda (_) (setf (aref prime-table i) (1+ (incf i)))) prime-table ))
    (loop
      for i from 0 below (length prime-table)
      when (not (null (aref prime-table i)))
        do (loop
             for j from (+ i (aref prime-table i)) below (length prime-table) by (aref prime-table i)
             do (setf (aref prime-table j) nil))
      finally (return prime-table))))


(defun prime-factors (x)
  (let ((table     (prime-table x))
        (current-x x)
        (factors))
    (loop
      for prime across table
      when (not (null prime))
        do (loop
             while (eq 0 (mod current-x prime))
             do (push prime factors)
             do (setf current-x (floor current-x prime)))
      finally (return (nreverse factors)))))
