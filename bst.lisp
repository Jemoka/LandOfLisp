(setf *random-state* (make-random-state t))

(defparameter **arrsz** 300)
(defparameter **anarray** (make-array **arrsz**))

(loop 
  for i from 0 to (- **arrsz** 1)
  do (setf (aref **anarray** i) (random 3500)))

(sort **anarray** #'<)

(defun bst (target arr mn mx)
  (princ (subseq arr mn mx))
  (fresh-line)
  (if (and (eq (- mx mn) 1) (eq (eq (aref arr 0) target) nil)) '(NO.)
    (let* ((n (ash (+ mn mx) -1)) (v (aref arr n)))
      (cond 
        ((= v target) `(HORRAY! Your thing is found at pos. ,n))
        ((< v target) (bst target arr n mx))
        ((> v target) (bst target arr mn n))
        ))))

(princ (string-trim "()" (princ-to-string (bst 82 **anarray** 0 (length **anarray**)))))

