(require 'url)

;; Modify below vars as suits your needs
(defvar github/default-username "bjuvensjo")
(defvar github/url-rest-api "https://api.github.com/users")

(defun github/clone-repo (clone-url out-dir)
  "Clone GitHub repo. Requires configured oauth, i.e. git config --global github.oauth-token <your token>"
  (interactive
   (let ((user (read-string (concat "User (" github/default-username "): ") nil nil github/default-username)))
     (list
      (replace-regexp-in-string "^[^:]+: *" "" (completing-read "Repo: " (github/get-repos user)))
      (read-directory-name "Output dir: "))))
  (let ((default-directory out-dir))
    (async-shell-command (concat "git clone " clone-url)))
  (dired out-dir))

(defun github/get-repos (user)
  "Get GitHub repos. Requires configured oauth, i.e. git config --global github.oauth-token <your token>"
  (let ((url-request-method "GET")
        (url-request-extra-headers
         (list (cons "Content-Type" "application/json")
               (cons "Authorization" (concat "token " (shell-command-to-string "git config --global --get github.oauth-token  | tr -d '\n\r'"))))))
    (save-excursion
      (switch-to-buffer (url-retrieve-synchronously (concat github/url-rest-api "/" user "/repos")))
      (beginning-of-buffer)
      (let ((repos (list)))
        (while (re-search-forward "\"name\":\"\\([^\"]+\\)\".*?\"clone_url\":\"\\([^\"]+\\)\"" nil t)
          (add-to-list 'repos (concat (match-string 1) ": " (match-string 2))))
        (kill-buffer)
        (sort repos 'string<)))))
