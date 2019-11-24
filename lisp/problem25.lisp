(defun rnd-permu (xs)
  (loop
    for i from (length xs) downto 2
    do (rotatef (car (nthcdr (random i) xs))
                (car (nthcdr (1- i)     xs)))
    finally (return xs)))
