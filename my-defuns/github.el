(require 'url)

;; Modify below vars as suits your needs
(defvar github/default-username "bjuvensjo")
(defvar github/url-rest-api "https://api.github.com/users")

(defun github/clone-repo (username password repo out-dir)
  "Clone GitHub repo."
  (interactive
   (let ((username (read-string (concat "Username (" github/default-username "): ") nil nil github/default-username))
         (password (read-passwd "Password: "))
         (user (read-string (concat "User (" github/default-username "): ") nil nil github/default-username)))
     (list
      username
      password
      (completing-read "Repo: " (github/get-repos username password user))
      (read-directory-name "Output dir: "))))
  (let ((default-directory out-dir))
    (async-shell-command (concat "git clone " (replace-regexp-in-string "^[^:]+: *" "" repo))))
  (dired out-dir))

(defun github/get-repos (username password user)
  "Get GitHub repos."
  (let ((url-request-method "GET")
        (url-request-extra-headers
         (list (cons "Content-Type" "application/json")
               (cons "Authorization" (concat "Basic " (base64-encode-string (concat username ":" password)))))))
    (save-excursion
      (switch-to-buffer (url-retrieve-synchronously (concat github/url-rest-api "/" user "/repos")))
      (beginning-of-buffer)
      (let ((repos (list)))
        (while (re-search-forward "\"name\":\"\\([^\"]+\\)\".*?\"clone_url\":\"\\([^\"]+\\)\"" nil t)
          (add-to-list 'repos (concat (match-string 1) ": " (match-string 2))))
        (kill-buffer)
        (sort repos 'string<)))))
