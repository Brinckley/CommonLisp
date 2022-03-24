(defun mv-return(n)
        (values (func-bind (- n 1)) (func-bind (- n 2)) (func-bind (- n 3))))

(defun func-bind(n)
        (if (< n 3)
                n
                (multiple-value-bind (y1 y2 y3) (mv-return n) 
                        (+ y1 y2 y3))))