(provide 'personal-utils)

(defun add-to-list-multiple (list elements-to-add)
  "Add multiple elements to a list at once."
  (dolist (item elements-to-add)
    (add-to-list list item)))
