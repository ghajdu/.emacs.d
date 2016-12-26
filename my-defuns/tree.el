;;; tree.el --- Insert a tree listing

;;; Commentary:

;;; Code:

;; Modify below vars as suits your needs
(defvar mbj/tree-ignored-names '(".git" "target" ".DS_Store" ".settings" ".idea" "*.iml" "*.iws"))

(defun mbj/tree (all dir)
  "Insert a tree listing of ALL in the specified DIR in current buffer.
If called without a prefix argument,
the files specified in the variable mbj/tree-ignored-names are ignored.
If called with a prefix argument,
all files are listed."
  (interactive "P\nD")
  (save-excursion
    (cl-labels ((inner (ignored-names name)
                       (if (not ignored-names)
                           nil
                         (if (string-match-p (car ignored-names) name)
                             t
                           (inner (cdr ignored-names) name))))
                (ignore-name (name)
                             (inner mbj/tree-ignored-names name))
                (tree (dir prefix)
                      (let ((absolute-files (directory-files dir t ".*[[:alnum:]].*"))
                            (files (directory-files dir nil ".*[[:alnum:]].*")))
                        (while files
                          (when (or all (not (ignore-name (car files))))
                            (progn
                              (insert (concat prefix (car files) "\n"))
                              (when (car (file-attributes (car absolute-files)))
                                (tree (car absolute-files) (concat "| " prefix)))))
                          (setq files (cdr files))
                          (setq absolute-files (cdr absolute-files))))))
      (tree dir "|____"))))

(provide 'tree)

;;; tree.el ends here
