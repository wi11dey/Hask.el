(defun hask-exec (command)
  (hask--exec command)
  (insert (read (hask--exec "propertize it"))))
