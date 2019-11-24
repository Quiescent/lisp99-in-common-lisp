(defun rnd-select (xs count)
  (let ((len (length xs)))
    (labels ((rnd-select-iter (acc i)
               (if (eq i 0)
                   acc
                   (rnd-select-iter (cons (nth (random len) xs) acc)
                                    (1- i)))))
      (rnd-select-iter nil count))))
