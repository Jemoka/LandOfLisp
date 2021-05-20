(defparameter *assos* '((living-room (toodle toodle doodie doo))
                        (chicken (noodle))))

(defun roomsay (loc path)
  `(here is a ,(caadr (assoc loc path)) in the room.)
)

