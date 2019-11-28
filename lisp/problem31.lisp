(defun coprime (x y)
  (let ((a (min x y))
        (b (max x y)))
    (labels
        ((iter (i acc)
           (cond ((eq i a)                                acc)
                 ((and (eq 0 (mod b i)) (eq 0 (mod a i))) (iter (1+ i) i))
                 (t                                       (iter (1+ i) acc)))))
      (eq 1 (iter 1 1)))))
