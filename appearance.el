;; Highlight current line
(global-hl-line-mode 1)

;; Set custom theme path
(setq custom-theme-directory (concat emacs.d-directory "/themes"))

(dolist
    (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))

(defun use-presentation-theme ()
  (interactive)
  (load-theme 'prez))

(defun use-default-theme ()
  (interactive)
  (load-theme 'default-black))

;; (use-default-theme)
(when is-mac 
  (load-theme 'leuven))

;; org-mode colors
;; (setq org-todo-keyword-faces
;;       '(
;;         ("INPR" . (:foreground "yellow" :weight bold))
;;         ("DONE" . (:foreground "green" :weight bold))
;;         ("IMPEDED" . (:foreground "red" :weight bold))
;;        ))

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; Unclutter the modeline
(require 'diminish)
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
(eval-after-load "eldoc" '(diminish 'eldoc-mode))
;; (eval-after-load "paredit" '(diminish 'paredit-mode))
;; (eval-after-load "tagedit" '(diminish 'tagedit-mode))
;; (eval-after-load "elisp-slime-nav" '(diminish 'elisp-slime-nav-mode))
;; (eval-after-load "skewer-mode" '(diminish 'skewer-mode))
;; (eval-after-load "skewer-css" '(diminish 'skewer-css-mode))
;; (eval-after-load "skewer-html" '(diminish 'skewer-html-mode))
(eval-after-load "smartparens" '(diminish 'smartparens-mode))
;; (eval-after-load "guide-key" '(diminish 'guide-key-mode))
(eval-after-load "magit" '(diminish 'magit-auto-revert-mode))
;; (eval-after-load "whitespace-cleanup-mode" '(diminish 'whitespace-cleanup-mode))
(when is-mac (eval-after-load "subword" '(diminish 'subword-mode)))

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(rename-modeline "js2-mode" js2-mode "JS2")
(rename-modeline "clojure-mode" clojure-mode "Clj")

(provide 'appearance)
