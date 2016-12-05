(require 'url)

;; Modify below vars as suits your needs
(defvar bitbucket/default-username "ei4577")
(defvar bitbucket/project-keys-file "~/.bitbucket-project-keys")
(defvar bitbucket/url-rest-api "http://cuso.edb.se/stash/rest/api/1.0/")

(defun bitbucket/get-project-keys ()
  "Gets the Bitbucket projects keys that are 'cached' in bitbucket/project-keys-file"
  (split-string (with-temp-buffer
                  (insert-file-contents bitbucket/project-keys-file)
                  (buffer-string)) "\n" t))

(defun bitbucket/get-clone-commands ()
  (goto-char 0)
  (let ((commands (list)))
    (while (re-search-forward "clone.*?\"\\(http:.*?git\\)" nil t)
      (let* ((repo-clone-url (match-string 1))
             (repo-name (replace-regexp-in-string ".*/" "" repo-clone-url))
             (repo-clone-dir (replace-regexp-in-string (regexp-quote ".") "/" (replace-regexp-in-string ".git$" "" repo-name))))
        (add-to-list 'commands (concat "git clone " repo-clone-url " " repo-clone-dir))))
    (mapconcat 'identity commands "\n")))

(defun bitbucket/clone-repos (project repo-regexp out-dir username password)
  "Clone Bitbucket repos."
  (interactive
   (list
    (replace-regexp-in-string ":.*" "" (completing-read "Project: " (bitbucket/get-project-keys)))
    (read-string "Repo regexp (.*): " nil nil ".*")
    (read-directory-name "Output dir: ")
    (read-string (concat "Username (" bitbucket/default-username "): ") nil nil bitbucket/default-username)
    (read-passwd "Password: ")))
  (let ((url-request-method "GET")
        (url-request-extra-headers
         (list (cons "Content-Type" "application/json")
               (cons "Authorization" (concat "Basic " (base64-encode-string (concat username ":" password)))))))
    (save-excursion
      (url-retrieve (concat bitbucket/url-rest-api "projects/" project "/repos?limit=1000")
                    (lambda (status out-dir repo-regexp)
                      (switch-to-buffer (current-buffer))
                      (mkdir out-dir t)
                      (let ((default-directory out-dir))
                        (async-shell-command (bitbucket/get-clone-commands)))
                      ;;(kill-buffer)
                      (dired out-dir))
                    (list (if (string-suffix-p "/" out-dir) out-dir (concat out-dir "/")) repo-regexp)))))

(defun bitbucket/get-projects ()
  (goto-char 0)
  (let ((projects (list)))
    (while (re-search-forward "\"key\":\"\\([^\"]+\\)\".*?\"name\":\"\\([^\"]+\\)" nil t)
      (let* ((key (match-string 1))
             (name (match-string 2)))
        (add-to-list 'projects (concat key ": " name))))
    (mapconcat 'identity projects "\n")))

(defun bitbucket/update-project-keys (username password)
  "Updates the Bitbucket projects keys that are 'cached' in bitbucket/project-keys-file"    
  (interactive
   (list
    (read-string (concat "Username (" bitbucket/default-username "): ") nil nil bitbucket/default-username)
    (read-passwd "Password: ")))
  (let ((url-request-method "GET")
        (url-request-extra-headers
         (list (cons "Content-Type" "application/json")
               (cons "Authorization" (concat "Basic " (base64-encode-string (concat username ":" password)))))))
    (save-excursion
      (url-retrieve (concat bitbucket/url-rest-api "projects?limit=1000")
                    (lambda (status)
                      (switch-to-buffer (current-buffer))
                      (let ((projects (bitbucket/get-projects)))
                        (erase-buffer)
                        (insert projects)
                        (sort-lines nil (point-min) (point-max))                        
                        (write-file bitbucket/project-keys-file)
                        (kill-buffer)
                        ))
                    ))))

(provide 'bitbucket)