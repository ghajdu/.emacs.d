(defun mbj/tree (dir)
  "
Inserts a tree listing of the specified dir in current buffer)
"
  (interactive "D")  
  (defun inner-tree(dir prefix)
    (let ((absolute-files (directory-files dir t ".*[a-zA-Z].*"))
          (files (directory-files dir nil ".*[a-zA-Z].*")))
      (while files
        (insert prefix)
        (insert "____")
        (insert (car files))
        (insert "\n")
        (when (car (file-attributes (car absolute-files)))
          (inner-tree (car absolute-files) (concat prefix " |")))
        (setq files (cdr files))
        (setq absolute-files (cdr absolute-files)))))
  (inner-tree dir "|"))

(provide 'mbj/tree)
