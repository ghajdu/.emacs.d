(defun guest/insert-count (count)
  (if (not (search-forward-regexp " *([0-9]+) *$" (line-end-position) t))
      (search-forward-regexp " *$" (line-end-position) t))
  (replace-match (concat " (" (number-to-string count) ")")))

(defun guest/count ()
  "Count guests per section and inserts number in section headers.
The sum of guests is placed in the first header."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (let ((sum 0))
      (while (search-forward-regexp "^\\*\\*" (point-max) t)
        (forward-line)
        (let* ((b (point))
               (e (save-excursion
                    (if (search-forward-regexp "^\\*\\*" (point-max) t)
                        (- (point) 2)
                      (point-max))))
               (count (count-matches "[[:alpha:]]+"  b e)))
          (previous-line)
          (guest/insert-count count)
          (setq sum (+ sum count))))
      (beginning-of-buffer)
      (guest/insert-count sum))))
