;;; sort-and-delete-duplicate-lines.el --- Sort and delete duplicate lines

;;; Commentary:

;;; Code:

(defun sort-and-delete-duplicate-lines()
  "Sort and delete duplicate lines in buffer."
  (interactive)
  (save-excursion
    (sort-lines nil (point-min) (point-max))
    (delete-duplicate-lines (point-min) (point-max))))

(provide 'sort-and-delete-duplicate-lines)

;;; sort-and-delete-duplicate-lines.el ends here
