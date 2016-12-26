;;; ido-find-resource.el --- Find resource in a root

;;; Commentary:

;;; Code:

(require 'popup)

(defvar my-ido-find-resource-root nil)
(defvar my-ido-find-resource-filter "\\( -name \"lib\" -o -name \"*.git\" -o -name \"*.svn\" -o -name \"*.settings\" -o -name \"*target\" -o -name \"*node_modules\" -o -name \"*classes\" \\) -prune -o -type f ! \\( -name \"*.class\" -o -name \"*.jar\" -o -name \".DS_Store\" -o -name \".classpath\" -o -name \".project\" \\)")

(defun my-ido-set-resource-root ()
  "Use ido to set the resource root."
  (interactive)
  (setq my-ido-find-resource-root (ido-read-directory-name "Resource root: ")))

(defun my-ido-find-resource ()
  "Use ido to find a file from the resource root."
  (interactive)
  (unless my-ido-find-resource-root (my-ido-set-resource-root))
  (cl-labels ((my-ido-find (root pattern prompt)
                 (let (resourceFiles tbl)
                   ;; get resource files
                   (setq resourceFiles
                         (split-string
                          (shell-command-to-string
                           (concat "find "
                                   root
                                   " "
                                   pattern
                                   " -print")) "\n"))
                   ;;(print resourceFiles)
                   ;; populate hash table (display repr => path)
                   (setq tbl (make-hash-table :test 'equal))
                   (let (ido-list)
                     (mapc (lambda (path)
                             (let ((key
                                    (->> path
                                         ;; format path for display in ido list
                                         (replace-regexp-in-string "\\(.*?\\)\\([^/]+?\\)$" "\\2")
                                         ;; strip root
                                         (replace-regexp-in-string root "")
                                         ;; remove trailing | or /
                                         (replace-regexp-in-string "\\(|\\|/\\)$" ""))))
                               (puthash key path tbl)
                               (push key ido-list)))
                           resourceFiles)
                     (gethash (popup-menu* ido-list :isearch t :isearch-cursor-color "dark red") tbl)))))
    (find-file (my-ido-find my-ido-find-resource-root my-ido-find-resource-filter "Name:"))))


;; bind keys for quick access
;;(global-set-key (kbd "H-o") 'my-ido-find-resource)
;;(global-set-key (kbd "H-r") 'my-ido-set-resource-root)

(provide 'ido-find-resource)

;;; ido-find-resource.el ends here
