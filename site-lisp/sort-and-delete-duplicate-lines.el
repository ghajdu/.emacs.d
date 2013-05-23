(defun sort-and-delete-duplicate-lines()
  "Sort and delete duplicate lines in buffer"
  (interactive)
  (save-excursion
    (sort-lines nil (point-min) (point-max))
    (goto-char (point-min))
    (let ((previousLine nil) (currentLine nil))
      (while (< (point) (point-max))
	(setq previousLine currentLine)
	(save-excursion (setq currentLine (thing-at-point 'line)))
        ;; (message (concat previousLine "###"  currentLine))
	(if (string-equal previousLine currentLine)
            ;; (message currentLine))
	    (kill-line 1)
	  (forward-line 1))))))

(provide 'sort-and-delete-duplicate-lines)
