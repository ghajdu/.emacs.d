;; I don't need to kill emacs that easily
;; the mnemonic is C-x REALLY QUIT
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)

(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "M-O") 'my-ido-set-resource-root)
(global-set-key (kbd "M-o") 'my-ido-find-resource)

;; Set modifier keys
(setq ns-alternate-modifier nil) ; alt
(setq ns-command-modifier 'meta) ; cmd
(setq ns-control-modifier 'control)  ; ctrl
(setq ns-function-modifier 'hyper) ; fn
(setq ns-right-alternate-modifier 'left) ; right alt
(setq ns-right-command-modifier 'super) ; right cmd

;; Completion that uses many different methods to find options.
;(global-set-key (kbd "C-.") 'hippie-expand-no-case-fold)
;(global-set-key (kbd "C-:") 'hippie-expand-lines)
;(global-set-key (kbd "C-,") 'completion-at-point)

;(require 'misc)
;(global-set-key (kbd "s-.") 'copy-from-above-command)

;; Smart M-x
;(global-set-key (kbd "M-x") 'smex)
;(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Use C-x C-m to do M-x per Steve Yegge's advice
;(global-set-key (kbd "C-x C-m") 'smex)

;; Expand region (increases selected region by semantic units)
(global-set-key (if is-mac (kbd "C-+") (kbd "C-'")) 'er/expand-region)

;; Experimental multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)

;; Mark additional regions matching current region
(global-set-key (kbd "M-ä") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "C-å") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-ä") 'mc/mark-next-like-this)
(global-set-key (kbd "C-Ä") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "M-å") 'mc/mark-all-in-region)

;; Symbol and word specific mark-more
;; (global-set-key (kbd "s-ä") 'mc/mark-next-word-like-this)
;; (global-set-key (kbd "s-å") 'mc/mark-previous-word-like-this)
;; (global-set-key (kbd "M-s-ä") 'mc/mark-all-words-like-this)
;; (global-set-key (kbd "s-Ä") 'mc/mark-next-symbol-like-this)
;; (global-set-key (kbd "s-Å") 'mc/mark-previous-symbol-like-this)
;; (global-set-key (kbd "M-s-ä") 'mc/mark-all-symbols-like-this)

;; Extra multiple cursors stuff
;; (global-set-key (kbd "C-~") 'mc/reverse-regions)
;; (global-set-key (kbd "M-~") 'mc/sort-regions)
;; (global-set-key (kbd "H-~") 'mc/insert-numbers)

;; (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; Set anchor to start rectangular-region-mode
(global-set-key (kbd "H-SPC") 'set-rectangular-region-anchor)

;; Replace rectangle-text with inline editing
(global-set-key (kbd "C-x r t") 'mc/edit-lines)

;; Quickly jump in document with ace-jump-mode
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
;; enable a more powerful jump back function from ace jump mode
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;; Perform general cleanup.
;(global-set-key (kbd "C-c n") 'cleanup-buffer)
;(global-set-key (kbd "C-c C-<return>") 'delete-blank-lines)

;; M-i for back-to-indentation
;(global-set-key (kbd "M-i") 'back-to-indentation)

;; Turn on the menu bar for exploring new modes
;(global-set-key (kbd "C-<f10>") 'menu-bar-mode)

;; Use shell-like backspace C-h, rebind help to F1
;(define-key key-translation-map [?\C-h] [?\C-?])
;(global-set-key (kbd "<f1>") 'help-command)

;(global-set-key (kbd "M-h") 'kill-region-or-backward-word)

;; Transpose stuff with M-t
;(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
;(global-set-key (kbd "M-t l") 'transpose-lines)
;(global-set-key (kbd "M-t w") 'transpose-words)
;(global-set-key (kbd "M-t s") 'transpose-sexps)
;(global-set-key (kbd "M-t p") 'transpose-params)

;; Change next underscore with a camel case
;(global-set-key (kbd "C-c C--") 'replace-next-underscore-with-camel)
;(global-set-key (kbd "M-s M--") 'snakeify-current-word)

;; Killing text
;(global-set-key (kbd "C-S-k") 'kill-and-retry-line)
;(global-set-key (kbd "C-w") 'kill-region-or-backward-word)
;(global-set-key (kbd "C-c C-w") 'kill-to-beginning-of-line)

;; Use M-w for copy-line if no active region
;(global-set-key (kbd "M-w") 'save-region-or-current-line)
;(global-set-key (kbd "M-W") '(lambda () (interactive) (save-region-or-current-line 1)))

;; Make shell more convenient, and suspend-frame less
;;(global-set-key (kbd "C-z") 'shell)
(global-set-key (kbd "C-x M-z") 'suspend-frame)

;; Zap to char
;(global-set-key (kbd "M-z") 'zap-up-to-char)
;(global-set-key (kbd "s-z") (lambda (char) (interactive "cZap up to char backwards: ") (zap-up-to-char -1 char)))

;(global-set-key (kbd "M-Z") 'zap-to-char)
;(global-set-key (kbd "s-Z") (lambda (char) (interactive "cZap to char backwards: ") (zap-to-char -1 char)))

;; iy-go-to-char - like f in Vim
;(global-set-key (kbd "M-m") 'jump-char-forward)
;(global-set-key (kbd "M-M") 'jump-char-backward)
;(global-set-key (kbd "s-m") 'jump-char-backward)

;; vim's ci and co commands
;(global-set-key (kbd "M-I") 'change-inner)
;(global-set-key (kbd "M-O") 'change-outer)

;(global-set-key (kbd "s-i") 'copy-inner)
;(global-set-key (kbd "s-o") 'copy-outer)

;; Create new frame
(define-key global-map (kbd "C-x C-n") 'make-frame-command)

;; Jump to a definition in the current file. (This is awesome)
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; File finding
;(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
;(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
;(global-set-key (kbd "C-x C-p") 'find-or-create-file-at-point)
;(global-set-key (kbd "C-x M-p") 'find-or-create-file-at-point-other-window)
;(global-set-key (kbd "C-c y") 'bury-buffer)
;(global-set-key (kbd "C-c r") 'revert-buffer)
;(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; toggle two most recent buffers
(fset 'quick-switch-buffer [?\C-x ?b return])
;;(global-set-key (kbd "C-b") 'quick-switch-buffer)
(global-set-key (kbd "C-z") 'quick-switch-buffer)

;; Revert without any fuss
;(global-set-key (kbd "M-<escape>")
;                (lambda () (interactive) (revert-buffer t t)))

;; Edit file with sudo
;(global-set-key (kbd "M-s e") 'sudo-edit)

;; Copy file path to kill ring
(global-set-key "\C-cp" 'copy-full-path-to-kill-ring)

;; Window switching
;(windmove-default-keybindings) ;; Shift+direction
;(global-set-key (kbd "C-x -") 'rotate-windows)
;(global-set-key (kbd "C-x C--") 'toggle-window-split)
;(global-unset-key (kbd "C-x C-+")) ;; don't zoom like this

;(global-set-key (kbd "C-x 3") 'split-window-right-and-move-there-dammit)

;; Add region to *multifile*
;(global-set-key (kbd "C-!") 'mf/mirror-region-in-multifile)

;; Indentation help
;(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))

;; Help should search more than just commands
;(global-set-key (kbd "<f1> a") 'apropos)

;; Should be able to eval-and-replace anywhere.
;(global-set-key (kbd "C-c C-e") 'eval-and-replace)

;; Navigation bindings
(global-set-key [remap goto-line] 'goto-line-with-feedback)

;(global-set-key (kbd "<prior>") 'beginning-of-buffer)
;(global-set-key (kbd "<home>") 'beginning-of-buffer)
;(global-set-key (kbd "<next>") 'end-of-buffer)
;(global-set-key (kbd "<end>") 'end-of-buffer)
;(global-set-key (kbd "M-p") 'backward-paragraph)
;(global-set-key (kbd "M-n") 'forward-paragraph)

;(global-set-key (kbd "M-<up>") 'smart-up)
;(global-set-key (kbd "M-<down>") 'smart-down)
;(global-set-key (kbd "M-<left>") 'smart-backward)
;(global-set-key (kbd "M-<right>") 'smart-forward)

;; Find files by name and display results in dired
;(global-set-key (kbd "M-s f") 'find-name-dired)
(key-chord-define-global "ff" 'find-name-dired)
(key-chord-define-global "fg" 'iy-go-to-char)
(key-chord-define-global "df" 'iy-go-to-char-backward)
(key-chord-define-global ";;" "\C-e;")
(key-chord-define-global ",." "{};\C-b\C-b")
(key-chord-define-global ".-" "[]\C-b")
(key-chord-define-global "\'\'" "''\C-b")
(key-chord-define-global "\"\"" "\"\"\C-b")

(global-set-key (kbd "C-c j") 'zencoding-expand-line)

;; Webjump let's you quickly search google, wikipedia, emacs wiki
;(global-set-key (kbd "C-x g") 'webjump)
;(global-set-key (kbd "C-x M-g") 'browse-url-at-point)

;; Completion at point
;(global-set-key (kbd "C-<tab>") 'completion-at-point)

;; Like isearch, but adds region (if any) to history and deactivates mark
;(global-set-key (kbd "C-s") 'isearch-forward-use-region)
;(global-set-key (kbd "C-r") 'isearch-backward-use-region)

;; Like isearch-*-use-region, but doesn't fuck with the active region
;(global-set-key (kbd "C-S-s") 'isearch-forward)
;(global-set-key (kbd "C-S-r") 'isearch-backward)

;; Move more quickly
;(global-set-key (kbd "C-S-n") (lambda () (interactive) (ignore-errors (next-line 5))))
;(global-set-key (kbd "C-S-p") (lambda () (interactive) (ignore-errors (previous-line 5))))
;(global-set-key (kbd "C-S-f") (lambda () (interactive) (ignore-errors (forward-char 5))))
;(global-set-key (kbd "C-S-b") (lambda () (interactive) (ignore-errors (backward-char 5))))

;(global-set-key (kbd "H-*") 'beginning-of-buffer) ;; H-p
;(global-set-key (kbd "H-n") 'end-of-buffer)
 
;; visual-regexp and visual-regexp-steroids
;;(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "M-&") 'vr/query-replace)
;; to use visual-regexp's isearch instead of the built-in regexp isearch, also include the following lines:
;(define-key global-map (kbd "C-r") 'vr/isearch-backward)
;(define-key global-map (kbd "C-s") 'vr/isearch-forward)

;; Yank selection in isearch
;(define-key isearch-mode-map (kbd "C-o") 'isearch-yank-selection)

;; Comment/uncomment block
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)

;; Eval buffer
;(global-set-key (kbd "C-c C-k") 'eval-buffer)

;; Create scratch buffer
(global-set-key (kbd "C-c b") 'create-scratch-buffer)

;; Move windows, even in org-mode
(global-set-key (kbd "<S-right>") 'windmove-right)
(global-set-key (kbd "<S-left>") 'windmove-left)
(global-set-key (kbd "<S-up>") 'windmove-up)
(global-set-key (kbd "<S-down>") 'windmove-down)

;; Magit
(global-set-key (kbd "C-x m") 'magit-status)
(autoload 'magit-status "magit")

;; Mu4e
;(global-set-key (kbd "C-x M") 'mu4e-up-to-date-status)

;; Clever newlines
(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)
(global-set-key (kbd "<M-return>") 'new-line-in-between)

;; Duplicate region
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

;; Line movement
(global-set-key (kbd "<M-down>") 'move-text-down)
(global-set-key (kbd "<M-up>") 'move-text-up)

;; Fold the active region
;(global-set-key (kbd "C-c C-f") 'fold-this-all)
;(global-set-key (kbd "C-c C-F") 'fold-this)
;(global-set-key (kbd "C-c M-f") 'fold-this-unfold-all)

;; Yank and indent
;(global-set-key (kbd "C-S-y") 'yank-unindented)

;; Toggle quotes
;(global-set-key (kbd "C-\"") 'toggle-quotes)

;; Sorting
;(global-set-key (kbd "M-s l") 'sort-lines)

;; Increase number at point (or other change based on prefix arg)
;(global-set-key (kbd "C-+") 'change-number-at-point)

;; Browse the kill ring
(global-set-key (kbd "C-x C-y") 'browse-kill-ring)

;; Buffer file functions
;(global-set-key (kbd "C-x t") 'touch-buffer-file)
;(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
;(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

;; Jump from file to containing directory
;(global-set-key (kbd "C-x C-j") 'dired-jump) (autoload 'dired-jump "dired")
;(global-set-key (kbd "C-x M-j") '(lambda () (interactive) (dired-jump 1)))

;; Easy-mode fullscreen rgrep
;(global-set-key (kbd "M-s s") 'git-grep-fullscreen)
;(global-set-key (kbd "M-s S") 'rgrep-fullscreen)

;; Multi-occur
;(global-set-key (kbd "M-s m") 'multi-occur)
;(global-set-key (kbd "M-s M") 'multi-occur-in-matching-buffers)

;; Display and edit occurances of regexp in buffer
(global-set-key (kbd "C-c o") 'occur)

;; Find file in project
;(global-set-key (kbd "C-x o") 'find-file-in-project)

;; Find file in project, with specific patterns
;(global-unset-key (kbd "C-x C-o")) ;; which used to be delete-blank-lines (also bound to C-c C-<return>)
;(global-set-key (kbd "C-x C-o ja") (ffip-create-pattern-file-finder "*.java"))
;(global-set-key (kbd "C-x C-o js") (ffip-create-pattern-file-finder "*.js"))
;(global-set-key (kbd "C-x C-o jp") (ffip-create-pattern-file-finder "*.jsp"))
;(global-set-key (kbd "C-x C-o cs") (ffip-create-pattern-file-finder "*.css"))
;(global-set-key (kbd "C-x C-o cl") (ffip-create-pattern-file-finder "*.clj"))
;(global-set-key (kbd "C-x C-o el") (ffip-create-pattern-file-finder "*.el"))
;(global-set-key (kbd "C-x C-o md") (ffip-create-pattern-file-finder "*.md"))
;(global-set-key (kbd "C-x C-o rb") (ffip-create-pattern-file-finder "*.rb"))
;(global-set-key (kbd "C-x C-o or") (ffip-create-pattern-file-finder "*.org"))
;(global-set-key (kbd "C-x C-o ph") (ffip-create-pattern-file-finder "*.php"))
;(global-set-key (kbd "C-x C-o tx") (ffip-create-pattern-file-finder "*.txt"))
;(global-set-key (kbd "C-x C-o vm") (ffip-create-pattern-file-finder "*.vm"))
;(global-set-key (kbd "C-x C-o xm") (ffip-create-pattern-file-finder "*.xml"))
;(global-set-key (kbd "C-x C-o pr") (ffip-create-pattern-file-finder "*.properties"))
;(global-set-key (kbd "C-x C-o in") (ffip-create-pattern-file-finder "*.ini"))
;(global-set-key (kbd "C-x C-o gr") (ffip-create-pattern-file-finder "*.groovy"))

;; View occurrence in occur mode
;(define-key occur-mode-map (kbd "v") 'occur-mode-display-occurrence)
;(define-key occur-mode-map (kbd "n") 'next-line)
;(define-key occur-mode-map (kbd "p") 'previous-line)


;; Sort and delete duplicate lines in buffer"
(global-set-key (kbd "<f9>") 'sort-and-delete-duplicate-lines)

;; Run current file
(global-set-key (kbd "<f7>") 'run-current-file)

;; Toggle window dedicated
(global-set-key (kbd "C-c t") 'toggle-window-dedicated)

;; Toggle between source and test in other window
(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "C-c C-j") 'jump-between-source-and-test-files-other-window))

;; Switch to next frame
(global-set-key (kbd "M-§") 'ns-next-frame)

(provide 'key-bindings)
