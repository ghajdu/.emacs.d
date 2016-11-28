;; Modify below vars as suits your needs
(defvar mbj/tree-ignored-names '(".git" "target" ".DS_Store" ".settings" ".idea" "*.iml" "*.iws"))

(defun mbj/tree (all dir)
  "Inserts a tree listing of the specified dir in current buffer. 
If called without a prefix argument, the files specified in the variable mbj/tree-ignored-names are ignored.
If called with a prefix argument, all files are listed.
"
  (interactive "P\nD")
  (defun ignore-name (name)
    (defun inner (ignored-names name)
      (if (not ignored-names)
          nil
        (if (string-match-p (car ignored-names) name)
            t
          (inner (cdr ignored-names) name))))
    (inner mbj/tree-ignored-names name))  
  (defun tree (dir prefix)
    (let ((absolute-files (directory-files dir t ".*[[:alnum:]].*"))
          (files (directory-files dir nil ".*[[:alnum:]].*")))
      (while files
        (when (or all (not (ignore-name (car files))))
          (progn
            (insert (concat prefix (car files) "\n"))
            (when (car (file-attributes (car absolute-files)))
              (tree (car absolute-files) (concat "| " prefix)))))
        (setq files (cdr files))
        (setq absolute-files (cdr absolute-files)))))
  (save-excursion
    (tree dir "|____")))

(provide 'tree)
