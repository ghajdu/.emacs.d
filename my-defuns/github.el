;;; github.el --- Clone from github

;;; Commentary:

;;; Code:

(require 'url)

;; Modify below vars as suits your needs
(defvar github/default-username "bjuvensjo")
(defvar github/url-rest-api "https://api.github.com/users")

(defun github/clone-repo (clone-url out-dir)
  "Clone GitHub repo from CLONE-URL to OUT-DIR.  Requires configured oauth, i.e. git config --global github.oauth-token <your token>."
  (interactive
   (let ((user (read-string (concat "User (" github/default-username "): ") nil nil github/default-username)))
     (list
      (replace-regexp-in-string "^[^:]+: *" "" (completing-read "Repo: " (github/get-repos user)))
      (read-directory-name "Output dir: "))))
  (let ((default-directory out-dir))
    (async-shell-command (concat "git clone " clone-url)))
  (dired out-dir))

(defun github/get-repos (user)
  "Get GitHub repos for USER.  Requires configured oauth, i.e. git config --global github.oauth-token <your token>."
  (let ((url-request-method "GET")
        (url-request-extra-headers
         (list (cons "Content-Type" "application/json")
               (cons "Authorization" (concat "token " (shell-command-to-string "git config --global --get github.oauth-token  | tr -d '\n\r'"))))))
    (save-excursion
      (let ((repos (list))
            (url (concat github/url-rest-api "/" user "/repos?per_page=100")))
        (while url
          (switch-to-buffer (url-retrieve-synchronously url))
          (goto-char (point-min))
          (setq url (if (re-search-forward "Link: <\\(.*\\)>; rel=\"next\"" nil t)
                        (match-string 1)
                      '()))
          (while (re-search-forward "\"name\":\"\\([^\"]+\\)\".*?\"clone_url\":\"\\([^\"]+\\)\"" nil t)
            (add-to-list 'repos (concat (match-string 1) ": " (match-string 2))))
          (kill-buffer))
        (sort repos 'string<)))))

(provide 'github)

;;; github.el ends here


