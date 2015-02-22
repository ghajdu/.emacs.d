(defun mbj/tree (dir)
  "
Inserts a tree listing of the specified dir in current buffer)
"
  (interactive "D")  
  (defun inner-tree(dir depth)
    (let ((buffer "")
          (prefix "|")
          (n depth)
          (files (directory-files dir t ".*[a-zA-Z].*")))
      (while (> n 0)
        (setq prefix (concat prefix " |"))
        (setq n (- n 1)))
      (while files
        (setq buffer (concat buffer
                             prefix
                             "____"
                             (replace-regexp-in-string ".*[\\]" "" ;; windows
                                                       (replace-regexp-in-string ".*\/" "" (car files))) ;; posix
                             "\n"
                             (if (car (file-attributes (car files)))
                                 (inner-tree (car files) (+ 1 depth))
                               "")))
        (setq files (cdr files)))
      buffer))
  (insert (inner-tree dir 1)))

(provide 'mbj/tree')
