(defun goldbach-list (start end)
  (let ((real-start (if (oddp start) (1+ start) start)))
   (loop
     for i from real-start upto end by 2
     for (a . b) = (goldbach i)
     collect (format nil "~a = ~a + ~a" i a b) into res
     finally (return (format nil "~{~a~%~}" res)))))
