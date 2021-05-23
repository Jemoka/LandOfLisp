(defun hellothefrkcingworldagain ()
  (princ "hello the frking world")
  (princ #\newline)
  (let ((name (read-line)))
    (princ "nice to meet you, ") 
    (princ name)))

(hellothefrkcingworldagain)


