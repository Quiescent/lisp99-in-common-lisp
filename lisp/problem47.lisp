(defpackage :truth-table-operators
  (:use :cl)
  (:export parse-expression)
  (:export truth-table)
  (:export preprocess-expression))

(in-package :truth-table-operators)

(defvar operators '(and or xor nand nor equ))

(defun parse-arguments (expression)
  (loop
    with arguments
    for token-list on expression
    for token = (car token-list)
    if (listp token)
      do (return (cons (nreverse arguments) token-list))
    else
      do (push token arguments)))

(defun group-expression (expression)
  (if (null expression)
      nil
      (let ((first-operand  (car expression))
            (second-operand (cadr expression)))
        (cons (if (listp first-operand)
                  (group-expression first-operand)
                  first-operand)
              (if (null second-operand)
                  nil
                  (cons (cadr expression)
                        (group-expression (cddr expression))))))))

(defun preprocess-expression (expression)
  (let ((grouped (group-expression expression)))
    (labels ((iter (xs)
               (cond
                 ((atom xs) xs)
                 (t         (let ((first-operand (car xs)))
                              (cons (cadr xs)
                                    (cons (if (listp first-operand)
                                              (iter first-operand)
                                              first-operand)
                                          (list (iter (caddr xs))))))))))
      (iter grouped))))

(defun parse-expression (expression)
  (cond
    ((null expression) nil)
    ((and (listp expression)
          (listp (car expression)))
     (parse-expression (car expression)))
    ((and (listp expression)
          (not (member (car expression) operators)))
     `(,(car expression)      ; A variable
        ,@(parse-expression (cdr expression))))
    ((atom expression) expression)
    (t (let ((operand-one (parse-expression (cadr expression)))
             (operand-two (parse-expression (cddr expression))))
         ;; YUCK!  There must be a better way!?
         (if (and (listp operand-one)
                  (listp operand-two))
             `((funcall
                ,(switch-operator (car expression))
                ,@operand-one
                ,@operand-two))
             (if (listp operand-one)
                 `((funcall
                    ,(switch-operator (car expression))
                    ,@operand-one
                    ,operand-two))
                 (if (listp operand-two)
                     `((funcall
                        ,(switch-operator (car expression))
                        ,operand-one
                        ,@operand-two))
                     `((funcall
                        ,(switch-operator (car expression))
                        ,operand-one
                        ,operand-two)))))))))

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
    (format t "Expression: ~a" (car other-tokens))
    (eval `(lambda ,args ,@(parse-expression (preprocess-expression (car other-tokens)))))))

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
