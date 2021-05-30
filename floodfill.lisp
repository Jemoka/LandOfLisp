(setf *random-state* (make-random-state t))

(princ "Max nodes: ") (finish-output)
(defconstant MAX-NODES (read))
(princ "Num edges: ") (finish-output)
(defconstant NUM-EDGES (read))
(fresh-line)

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

(defun get-num-blobs (table)
  (let* ((nodes nil) (seen nil) (blobs 0)) 
    (maphash (lambda (x y) (push x nodes)) table)
    (mapcar (lambda (n)
              (when (not (member n seen))
                (incf blobs)
                (setf seen (concatenate 'list seen (get-connected n table)))))
              nodes)
    blobs))

(defparameter *tree* (make-hash-table))

(defun populate-tree (tree)
  (loop repeat NUM-EDGES do 
        (let* ((res (make-edge tree)) 
               (a (car res))
               (b (cdr res)))
          (push a (gethash b tree))
          (push b (gethash a tree)))))

(populate-tree *tree*)
(defparameter *num-blobs* (get-num-blobs *tree*))

(format t "~10t~a ~25t~a ~35t~a~%" "Max Nodes" "# Edges" "Blobs")
(format t "~10t~a ~25t~a ~35t~a~%~%" MAX-NODES NUM-EDGES *num-blobs*)

