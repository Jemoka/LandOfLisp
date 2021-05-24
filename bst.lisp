(setf *random-state* (make-random-state t))

(defparameter *arrsz* 1000)
(defparameter *anarray* (make-array *arrsz*))


(defun setarray () 
    (loop 
      for i from 0 to (- *arrsz* 1)
      do (setf (aref *anarray* i) (random 500)))

    (sort *anarray* #'<))


(defun bst (target arr mn mx)
  ;(princ (subseq arr mn mx))
  (fresh-line)
  (if (and (eq (- mx mn) 1) (eq (eq (aref arr 0) target) nil)) (values nil '(NO.))
    (let* ((n (ash (+ mn mx) -1)) (v (aref arr n)))
      (cond 
        ((= v target) (values t `(HORRAY! Your thing is found at pos. ,n)))
        ((< v target) (bst target arr n mx))
        ((> v target) (bst target arr mn n))
        ))))

(defparameter *success* 0)
(defparameter *failure* 0)

(loop for i below 100 do 
      (setarray)
      (multiple-value-bind (r a) (bst 57 *anarray* 0 (length *anarray*))
          (if r
            (setf *success* (1+ *success*)) 
            (setf *failure* (1+ *failure*)))))

(fresh-line)
(princ (string-trim "()" (princ-to-string `(Success rate ,(/ *success* (+ (float *success*) *failure*))))))

