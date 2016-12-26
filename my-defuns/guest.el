;;; guest.el --- Count guests

;;; Commentary:

;;; Code:

(defun guest/insert-count (count)
  "Insert COUNT."
  (if (not (search-forward-regexp " *([0-9]+) *$" (line-end-position) t))
      (search-forward-regexp " *$" (line-end-position) t))
  (replace-match (concat " (" (number-to-string count) ")")))

(defun guest/count ()
  "Count guests per section and insert number in section headers.
The sum of guests is placed in the first header."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((sum 0))
      (while (search-forward-regexp "^\\*\\*" (point-max) t)
        (forward-line)
        (let* ((b (point))
               (e (save-excursion
                    (if (search-forward-regexp "^\\*\\*" (point-max) t)
                        (- (point) 2)
                      (point-max))))
               (count (count-matches "[[:alpha:]]+"  b e)))
          (forward-line -1)
          (guest/insert-count count)
          (setq sum (+ sum count))))
      (goto-char (point-min))
      (guest/insert-count sum))))

;;; guest.el ends here
