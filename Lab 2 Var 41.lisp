(defun treelist (tree)
        (cond 
                ((null tree) nil)
                ((atom tree) (list tree))
                (t (append (treelist (first tree)) (treelist (rest tree))))))

(defun compare (list1 list2)
        (if (and (null list1) (null list2)) t
        (if (or (null list1) (null list2)) nil 
                (if (and (atom list1) (atom list2)) (= list1 list2) 
                (and (compare (first list1) (first list2)) (compare (rest list1) (rest list2)))))))

(defun tree-similar-p (tree1 tree2)
        (compare (treelist tree1) (treelist tree2)))