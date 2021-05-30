(defconstant MAX-NODES 12)
(defconstant NUM-EDGES 12)

(defun make-edge (table)
  (let ((a (random MAX-NODES)) (b (random MAX-NODES)))
    (if (not (or (member a (gethash b table)) (member b (gethash a table)) (= a b)))
            (cons a b)
            (make-edge table)))) ;; this should 1) not happen much 2) tail recursion baybee

(defun get-connected (node table &optional visited)
  (let ((c visited) (nodes (gethash node table))) 
    (setf c (concatenate 'list c nodes))
    (mapcar (lambda (n) 
              (unless (member n visited) 
                (setf c (remove-duplicates (concatenate 'list c (get-connected n table c))))))
            nodes)
    c))

(defparameter *tree* (make-hash-table))

(defun populate-tree (tree)
  (loop repeat NUM-EDGES do 
        (let* ((res (make-edge tree)) 
               (a (car res))
               (b (cdr res)))
          (push a (gethash b tree))
          (push b (gethash a tree)))))

(populate-tree *tree*)
;(get-connected 7 *tree*)

