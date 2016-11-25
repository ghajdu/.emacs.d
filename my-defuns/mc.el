(require 'json)
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
                      (mkdir out-dir t)
                      (let ((default-directory out-dir))
                        (async-shell-command (mbj/bitbucket-get-clone-commands)))
                      (kill-buffer)
                      (dired out-dir))
                    (list (if (string-suffix-p "/" out-dir) out-dir (concat out-dir "/")) repo-regexp)))))

(defun mbj/bitbucket-get-clone-commands ()
  (interactive)
  (goto-char 0)
  (let ((commands (list)))
    (while (re-search-forward "clone.*?\"\\(http:.*?git\\)" nil t)
      (let* ((repo-clone-url (match-string 1))
             (repo-name (replace-regexp-in-string ".*/" "" repo-clone-url))
             (repo-clone-dir (replace-regexp-in-string (regexp-quote ".") "/" repo-name)))
        (add-to-list 'commands (concat "git clone " repo-clone-url " " repo-clone-dir))))
    (mapconcat 'identity commands "\n")))

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
