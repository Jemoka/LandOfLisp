(defconstant MAX-NODES 12)
(defconstant NUM-EDGES 12)

(defun make-edge (table)
  (let ((a (random MAX-NODES)) (b (random MAX-NODES)))
    (if (not (or (member a (gethash b table)) (member b (gethash a table)) (= a b)))
            (cons a b)
            (make-edge table)))) ;; this should 1) not happen much 2) tail recursion baybee

(defun get-connected (node table &optional visited)
  (let* ((nodes (gethash node table)) (c (remove-if (lambda (n) (member n visited)) nodes)) (e nil)) 
    (mapcar (lambda (n) 
              (push n e)
              (setf e (concatenate 'list e (get-connected n table (concatenate 'list visited c e)))))
        c)
    e))

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

