(require 'json)
(require 's)
(require 'url)

;; Modify below vars as suits your needs
(defvar mbj/bitbucket-default-username "ei4577")
(defvar mbj/bitbucket-project-keys-file "~/.bitbucket-project-keys")
(defvar mbj/bitbucket-url-rest-api "http://cuso.edb.se/stash/rest/api/1.0/")

(defun mbj/bitbucket-clone-repos (project repo-regexp out-dir username password)
  "Clone Bitbucket repos."
  (interactive
   (list
    (completing-read "Project: " (mbj/bitbucket-get-project-keys))
    (read-string "Repo regexp (.*): " nil nil ".*")
    (read-directory-name "Output dir: ")
    (read-string (concat "Username (" mbj/bitbucket-default-username "): ") nil nil mbj/bitbucket-default-username)
    (read-passwd "Password: ")))
  (let ((url-request-method "GET")
        (url-request-extra-headers
         (list (cons "Content-Type" "application/json")
               (cons "Authorization" (concat "Basic " (base64-encode-string (concat username ":" password)))))))
    (save-excursion
      (url-retrieve (concat mbj/bitbucket-url-rest-api "projects/" project "/repos?limit=1000")
                    (lambda (status out-dir repo-regexp)
                      (switch-to-buffer (current-buffer))
                      (keep-lines "{.*" (point-min) (point-max))
                      (json-pretty-print-buffer)
                      (keep-lines ".*http.*git.*" (point-min) (point-max))
                      (replace-string "\\" "" nil (point-min) (point-max))                        
                      (goto-char 0)
                      (insert "cd " out-dir "\n")
                      (while (re-search-forward ".*\\(\"http:.*/\\)\\(.*\\)\\(\\.git.*\\)" nil t)
                        (let* ((repo-dir (s-replace-all '(("." . "/")) (match-string 2)))
                               (cmd (concat "git clone \\1\\2\\3 " repo-dir)))
                          (replace-match (if (s-match repo-regexp (match-string 2)) cmd (concat "#" cmd)) nil nil)))
                      (mkdir out-dir t)
                      (async-shell-command (buffer-string)))
                    (list out-dir repo-regexp)))))

(defun mbj/bitbucket-get-project-keys ()
  "Gets the Bitbucket projects keys that are 'cached' in mbj/bitbucket-project-keys-file"
  (interactive)
  (split-string (with-temp-buffer
                  (insert-file-contents mbj/bitbucket-project-keys-file)
                  (buffer-string)) "\n" t))

(defun mbj/bitbucket-update-project-keys (username password)
  "Updates the Bitbucket projects keys that are 'cached' in mbj/bitbucket-project-keys-file"
  (interactive
   (list
    (read-string (concat "Username (" mbj/bitbucket-default-username "): ") nil nil mbj/bitbucket-default-username)
    (read-passwd "Password: ")))
  (let ((url-request-method "GET")
        (url-request-extra-headers
         (list (cons "Content-Type" "application/json")
               (cons "Authorization" (concat "Basic " (base64-encode-string (concat username ":" password)))))))
    (save-excursion
      (url-retrieve (concat bitbucket-url-rest-api "projects?limit=1000")
                    (lambda (status)
                      (switch-to-buffer (current-buffer))
                      (keep-lines "{.*" (point-min) (point-max))
                      (mark-whole-buffer)
                      (kill-ring-save (point-min) (point-max))
                      (create-scratch-buffer)
                      (yank)
                      (json-pretty-print-buffer)
                      (keep-lines "key" (point-min) (point-max))
                      (while (re-search-forward ".*key\": \"\\(.*\\)\"" nil t)
                        (replace-match "\\1" nil nil))
                      (write-file mbj/bitbucket-project-keys-file)
                      (kill-buffer))))))

(provide 'mc)
