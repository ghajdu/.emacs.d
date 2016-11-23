(require 'json)
(require 's)
(require 'url)

;; Modify below vars as suits your needs
(defvar mbj/bitbucket-url-rest-api "http://cuso.edb.se/stash/rest/api/1.0/")
(defvar mbj/bitbucket-project-keys '("LPLAN"
                                     "LHAUTO"
                                     "LPLANO"
                                     "LPLANT"
                                     "SBAB-LP"
                                     "SBAB-LP-AUTOMATION"
                                     "SBAB-LP-SERVICE"
                                     "SBAB-LP-TMP"))

(defun mbj/bitbucket-clone-repos (project repo-regexp out-dir username password)
  "Clone Bitbucket repos."
  (interactive
   (list
    (completing-read "Project: " mbj/bitbucket-project-keys)
    (read-string "Repo regexp (.*): " nil nil ".*")
    (read-directory-name "Output dir: ")
    (read-string "Username (ei4577): " nil nil "ei4577")
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

;; (let ((url-request-method "GET")
;;       (url-request-extra-headers
;;        (list (cons "Authorization" bitbucket-authorization) (cons "Content-Type" "application/json;charset=UTF-8"))))
;;   (url-retrieve (concat bitbucket-url-rest-api "projects?limit=1000")
;;                 (lambda (status)
;;                   (switch-to-buffer (current-buffer))
;;                   (keep-lines "{.*" (point-min) (point-max))
;;                   (mark-whole-buffer)
;;                   (kill-ring-save (point-min) (point-max))
;;                   (create-scratch-buffer)
;;                   (yank)
;;                   (json-pretty-print-buffer)
;;                   (keep-lines "key" (point-min) (point-max))
;;                   (while (re-search-forward ".*key\": \\(.*\\)" nil t)
;;                     (replace-match "\\1" nil nil))
;;                   )))

(provide 'mc)
