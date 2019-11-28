(defun is-prime (x)
  (let ((prime-table (make-array (list (1- (floor x 2))))))
    (let ((i 0))
      (map-into prime-table (lambda (_) (setf (aref prime-table i) (1+ (incf i)))) prime-table ))
    (loop
      for i from 0 below (length prime-table)
      when (not (null (aref prime-table i)))
        do (if (eq 0 (mod x (aref prime-table i)))
               (return nil)
               (loop
                 for j from i below (length prime-table) by (aref prime-table i)
                 do (setf (aref prime-table j) nil)))
      finally (return t))))
