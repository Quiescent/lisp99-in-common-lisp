(defvar operators '(and or xor nand nor equ))

(defun parse-arguments (expression)
  (loop
    with arguments
    for token-list on expression
    for token = (car token-list)
    if (member token operators)
      do (return (cons (nreverse arguments) token-list))
    else
      do (push token arguments)))

(defun parse-expression (expression)
  (cond
    ((null expression) nil)
    ((not (member (car expression) operators))
     `(,(car expression) ; A variable
        ,@(parse-expression (cdr expression))))
    (t `((funcall
          ,(switch-operator (car expression))
          ,@(parse-expression (cadr expression))
          ,@(parse-expression (cddr expression)))))))

(defun switch-operator (operator)
  (cond
    ((eq operator 'and)  #'and/2)
    ((eq operator 'or)   #'or/2)
    ((eq operator 'nand) #'nand/2)
    ((eq operator 'nor)  #'nor/2)
    ((eq operator 'xor)  #'xor/2)
    ((eq operator 'equ)  #'equ/2)))

(defun evaluate-to-function (expression)
  (destructuring-bind (args . other-tokens) (parse-arguments expression)
    (eval `(lambda ,args ,@(parse-expression other-tokens)))))

(defun truth-table (expression)
  (let ((truth-function (evaluate-to-function expression)))
    (print truth-function)
    (format nil "~%~a ~a ~a~%~a ~a ~a~%~a ~a ~a~%~a ~a ~a"
            'false
            'false
            (boolish-to-symbol (funcall truth-function nil nil))
            'false
            'true
            (boolish-to-symbol (funcall truth-function nil t))
            'true
            'false
            (boolish-to-symbol (funcall truth-function t nil))
            'true
            'true
            (boolish-to-symbol (funcall truth-function t nil)))))

(defun boolish-to-symbol (x)
  (if x 'true 'false))

(defun and/2 (x y)
  (and x y))

(defun or/2 (x y)
  (or x y))

(defun nand/2 (x y)
  (not (and x y)))

(defun nor/2 (x y)
  (not (or x y)))

(defun xor/2 (x y)
  (or (and x       (not y))
      (and (not x) y)))

(defun equ/2 (x y)
  (eq x y))
