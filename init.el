; Set fringes
(set-fringe-style '(10 . 0))

;; Turn off mouse interface early in startup to avoid momentary display
;; (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; No scratch message
(setq initial-scratch-message nil)

;; This is not really good...
;; (setq warning-minimum-level :emergency)
(setq settings-dir (expand-file-name "settings" emacs.d-directory))

;; ;; Set up load path
(add-to-list 'load-path settings-dir)

(setq package-user-dir (expand-file-name "elpa" emacs.d-directory))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" emacs.d-directory))
(load custom-file)

;; Write backup files to own directory
(setq backup-directory-alist `(("." . ,(expand-file-name "backups"  user-emacs-directory))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Save point position between sessions
(save-place-mode 1)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))
(setq save-place-forget-unreadable-files nil)

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; ;; Setup packages
(require 'setup-package)

;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(;; gist    
     ;; whitespace-cleanup-mode
     ace-jump-mode
     browse-kill-ring
     change-inner
     cider
     clojure-mode
     clojure-mode-extra-font-locking
     company
     dash
     diminish
     dired-details
     elisp-slime-nav
     elmacro
     expand-region
     f
     find-file-in-project
     flx
     flx-ido
     flycheck
     flycheck-clojure
     flycheck-pos-tip
     groovy-mode
     groovy-mode
     guide-key
     htmlize
     hydra
     ido-at-point
     ido-ubiquitous
     ido-vertical-mode
     iy-go-to-char
     js2-mode
     json-mode
     key-chord
     magit
     markdown-mode
     move-text
     multiple-cursors
     paredit
     phi-search
     popup
     restclient
     s
     shell-command
     smartparens
     smex
     smooth-scrolling
     undo-tree
     visual-regexp
     wgrep
     yaml-mode
     yasnippet
     zencoding-mode
     zoom-frm)))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; Set up appearance early
(require 'appearance)

;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; Setup environment variables from the user's shell.
(when is-mac
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(setq shell-command-switch "-ic")

;; guide-key
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x v" "C-x 8" "C-x +"))
(guide-key-mode 1)
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'bottom)

;; ;; Setup extensions
(eval-after-load 'ido '(require 'setup-ido))
;; (eval-after-load 'org '(require 'setup-org))
(eval-after-load 'dired '(require 'setup-dired))
(eval-after-load 'magit '(require 'setup-magit))
(eval-after-load 'grep '(require 'setup-rgrep))
(eval-after-load 'multiple-cursors '(require 'setup-iy-go-to-char))
;; (eval-after-load 'shell '(require 'setup-shell))
(require 'setup-hippie)
(require 'key-chord)
(key-chord-mode 1)
(require 'setup-paredit)
;; (require 'setup-perspective)
(require 'setup-restclient)
(require 'setup-smartparens)
(require 'setup-yaml)
(require 'setup-yasnippet)
(require 'setup-ffip)
;; (require 'setup-html-mode)
(require 'setup-webjump)

;; Font lock dash.el
(eval-after-load "dash" '(dash-enable-font-lock))

;; Language specific setup files
(eval-after-load 'js2-mode '(require 'setup-js2-mode))
(eval-after-load 'clojure-mode '(require 'setup-clojure-mode))
(eval-after-load 'markdown-mode '(require 'setup-markdown-mode))

;; Load stuff on demand
;; (autoload 'skewer-start "setup-skewer" nil t)
;; (autoload 'skewer-demo "setup-skewer" nil t)
(autoload 'auto-complete-mode "auto-complete" nil t)
(eval-after-load 'flycheck '(require 'setup-flycheck))

;; Map files to modes
;; (require 'mode-mappings)

;; Highlight escape sequences
;; (require 'highlight-escape-sequences)
;; (hes-mode)
;; (put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face)

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" emacs.d-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; Functions (load all el-files in my-defuns-dir)
(setq defuns-dir (expand-file-name "my-defuns" emacs.d-directory))
(dolist (file (directory-files defuns-dir t "\\w+.el"))
  (when (file-regular-p file)
    (load file)))

(require 'expand-region)
(require 'multiple-cursors)
(require 'jump-char)
;; (require 'eproject)
(require 'visual-regexp)
(require 'wgrep)
;; (require 'smart-forward)
(require 'change-inner)
;; (require 'multifiles)

;; ;; Don't use expand-region fast keys
(setq expand-region-fast-keys-enabled t)

;; ;; Show expand-region command used
(setq er--show-expansion-message t)

;; ;; Fill column indicator
;; (require 'fill-column-indicator)
;; (setq fci-rule-color "#111122")

;; ;; Browse kill ring
(require 'browse-kill-ring)
(setq browse-kill-ring-quit-action 'save-and-restore)

;; Smart M-x is smart
(require 'smex)
(smex-initialize)

;; Zencoding
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes

;; elmacro
(require 'elmacro)
;; (elmacro-mode)

;; phi-search
(require 'phi-search)
(setq phi-search-limit           10000
      phi-search-case-sensitive  'guess) ;; You may also set “phi-search-case-sensitive” to ‘guess, to make phi-search case sensitive only when some upcase letters are in the query.


;; Setup key bindings
(require 'key-bindings)

;; Misc
;; ;; (require 'project-archetypes)
;; (require 'my-misc)
(when is-mac (require 'mac))

;; gpg
;; (setq epg-gpg-program "gpg2")
;;(setq epg-gpg-program "gpg")
;; (setenv "INSIDE_EMACS" (format "%s,comint" emacs-version))
;; (pinentry-start)
;; (setenv "GPG_AGENT_INFO" nil)

;; ;; Elisp go-to-definition with M-. and back again with M-,
(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
(add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t) (eldoc-mode 1)))

;; ;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;; ;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Conclude init by setting up specifics for the current user
;; (when (file-exists-p user-settings-dir)
;;   (mapc 'load (directory-files user-settings-dir nil "^[^#].*el$")))

;; Company mode everywhere
(add-hook 'after-init-hook 'global-company-mode)
(setq company-dabbrev-downcase nil)

;; Frame window size
(set-frame-parameter nil 'fullscreen 'fullboth)
;; (add-to-list 'initial-frame-alist '(width  . 166))
;; (add-to-list 'initial-frame-alist '(height . 47))

;; Git
(global-git-commit-mode)

;; xml indentation
(setq nxml-child-indent 4 nxml-attribute-indent 4)

;; indentation
(setq c-basic-offset 4)
