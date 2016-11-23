(defun mbj/read-string (file)
  "Read the content of a file into a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun mbj/write-string (file content)
  (with-temp-buffer
    (insert content)
    (when (file-writable-p file)
      (write-region (point-min)
                    (point-max)
                    file))))

(defun mbj/create-git-ignore (dir)
  "Creates a .gitignore file."
  (interactive "Ddir:")
  (mbj/write-string (concat dir ".gitignore")
                    (s-join "\n" '(".settings" "/.project" "/.classpath" ".DS_Store" ".idea" ".nrepl-port" "*.iml" "*.iws" "docs.txt"))))

(defun mbj/git-init (dir)
  "Initializes a git repository with a .gitignore file and master and develop branches."
  (interactive "D")
  (mbj/create-git-ignore dir)
  (shell-command "git init")
  (shell-command "git add .gitignore")
  (shell-command "git commit -am \"initial version\"")
  (shell-command "git branch develop")
  (shell-command "git checkout develop")
  (shell-command "git add --all")
  (shell-command "git commit -am \"initial develop version\""))

(setq pom-template (expand-file-name "my-defuns/pom-template.xml" emacs.d-directory))

(defun mbj/mp (base-dir group-id artifact-id version)
  "Creates a maven project and a git repository."
  (interactive "Dbase-dir:\nsgroup-id:\nsartifact-id:\nsversion (1.0.0-SNAPSHOT):")
  (let ((version (if (s-blank? (s-trim version))
                     "1.0.0-SNAPSHOT"
                   version)))
    (let ((out-dir (concat base-dir "/" artifact-id "/"))         
          (package-path (s-replace-all '(("." . "/")) group-id))                 
          (pom-content (s-replace-all (list (cons "###groupId###" group-id)
                                            (cons "###artifactId###" artifact-id)
                                            (cons "###version###" version))
                                      (mbj/read-string pom-template))))
      (mkdir out-dir t)
      (mbj/write-string (concat out-dir "pom.xml") pom-content)
      (dolist (dir '("src/main/java/" "src/main/resources/" "src/test/java/" "src/test/resources/"))
        (mkdir (concat out-dir dir package-path) t))
      (let ((default-directory out-dir))
        (mbj/git-init out-dir)))))

(provide 'mp)
