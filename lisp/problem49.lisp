(defun gray-code (x)
  (let ((latest 0)
        (codes  (list 0))
        (masks  (loop
                  for i below x
                  collect (ash 1 i))))
    (labels ((test-code (code)
               (when (not (member code codes))
                 (push code   codes)
                 (setf latest code)
                 (iter)
                 t))
             (iter ()
               (loop
                 for mask in masks
                 when (not (test-code (logior mask           latest)))
                   do (test-code (logand (lognot mask)  latest)))))
      (iter)
      (nreverse codes))))
