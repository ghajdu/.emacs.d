(defvar my-ido-find-resource-root nil)
(defvar my-ido-find-resource-filter "\\( -name \"lib\" -o -name \"*.git\" -o -name \"*.svn\" -o -name \"*.settings\" -o -name \"*target\" -o -name \"*classes\" \\) -prune -o -type f ! \\( -name \"*.class\" -o -name \"*.jar\" -o -name \".DS_Store\" -o -name \".classpath\" -o -name \".project\" \\)")

(defun my-ido-set-resource-root ()
  "Use ido to set the resource root."
  (interactive)
  (setq my-ido-find-resource-root (ido-read-directory-name "Resource root: ")))

(defun my-ido-find-resource ()
  "Use ido to find a file from the resource root."
  (interactive)
  (unless my-ido-find-resource-root (my-ido-set-resource-root))
  (defun find (root pattern prompt)
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
      ;;(prin1 resourceFiles)
      ;; populate hash table (display repr => path)
      (setq tbl (make-hash-table :test 'equal))
      (let (ido-list)
	(mapc (lambda (path)
		;; format path for display in ido list
		(setq key (replace-regexp-in-string "\\(.*?\\)\\([^/]+?\\)$" "\\2|\\1" path))
		;; strip root
		(setq key (replace-regexp-in-string root "" key))
		;; remove trailing | or /
		(setq key (replace-regexp-in-string "\\(|\\|/\\)$" "" key))
		(puthash key path tbl)
		(push key ido-list))
	      resourceFiles)
	;;(gethash (ido-completing-read prompt ido-list) tbl))))
        (gethash (popup-menu* ido-list) tbl))))
  (find-file (find my-ido-find-resource-root my-ido-find-resource-filter "Name:")))

;; bind keys for quick access
;(global-set-key (kbd "s-o") 'my-ido-find-resource)
;(global-set-key (kbd "s-r") 'my-ido-set-resource-root)


