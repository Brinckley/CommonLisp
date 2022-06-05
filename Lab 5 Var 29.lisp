(defclass polynom ()
        ((var-symbol :initarg :var :reader var)
        (term-list :initarg :terms :reader terms)))

(defun make-term (&key order coeff)
        (list order coeff))

(defun order (term) (first term))
(defun coeff (term) (second term))

(defgeneric zerop1 (arg)
        (:method ((n number))
                (zerop n)))

(defun same-variable-p (v1 v2)
        (and (symbolp v1) (symbolp v2) (eq v1 v2)))

(defmethod add2 ((n number) (p1 polynom))
        (make-instance 'polynom
                         :var (var p1)
                        :terms (add-number n (terms p1))))

(defun add-number (n l)
        (cond
                ((eq n 0) l)                                                        ; nothing to change....
                ((null l) (adjoin-term (make-term :order 0 :coeff n) '()))          ; nothing in poly => creating poly with only (order = 0 coeff = n)
                (t
                        (let ((t1 (first l)))
                                (cond 
                                        ((> (order t1) 0)                                         ; going down the polynom searching for 0 order (it is the smallest) 
                                                (adjoin-term t1
                                                (add-number n (rest l))))
                                        ((eq (order t1) 0) 
                                                (cond 
                                                        ((eq 0 (+ n (coeff t1))) '())
                                                        (t (adjoin-term (make-term :order 0 :coeff (+ n (coeff t1))) '())))
                                        )
                                )
                        )
                )
        )
)

(defun adjoin-term (term term-list)
        (if (zerop1 (coeff term))
                term-list               
                (cons term term-list)))
