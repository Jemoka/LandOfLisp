(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *num-node* 30)
(defparameter *num-edge* 45)
(defparameter *num-worm* 3)
(defparameter *cop-odds* 15)

(defun random-node () 
  (+ 1 (random *num-node*)))

(defun make-pair (a b)
  (unless (eql a b )
    (list (cons a b) (cons b a))))

