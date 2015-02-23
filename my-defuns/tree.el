(defun mbj/tree (dir)
  "
Inserts a tree listing of the specified dir in current buffer)
"
  (interactive "D")  
  (defun tree(dir prefix)
    (let ((absolute-files (directory-files dir t ".*[a-zA-Z].*"))
          (files (directory-files dir nil ".*[a-zA-Z].*")))
      (while files
        (insert (concat prefix (car files) "\n"))
        (when (car (file-attributes (car absolute-files)))
          (tree (car absolute-files) (concat "| " prefix)))
        (setq files (cdr files))
        (setq absolute-files (cdr absolute-files)))))
  (save-excursion
    (tree dir "|____")))

(provide 'mbj/tree)
