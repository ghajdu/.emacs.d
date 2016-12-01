(require 'git)

(defvar pom-template (expand-file-name "my-defuns/pom-template.xml" emacs.d-directory) "pom template file.")

(defun maven/mp (base-dir group-id artifact-id version)
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
                                      (with-temp-buffer
                                        (insert-file-contents pom-template)
                                        (buffer-string)))))
      (mkdir out-dir t)
      (with-temp-buffer
        (let ((file (concat out-dir "pom.xml")))
          (insert pom-content)
          (when (file-writable-p file)
            (write-region (point-min)
                          (point-max)
                          file))))
      (dolist (dir '("src/main/java/" "src/main/resources/" "src/test/java/" "src/test/resources/"))
        (mkdir (concat out-dir dir package-path) t))
      (let ((default-directory out-dir))
        (git/git-init out-dir)))))

(provide 'maven)
