;; Open drag and drop in same window/instance
;;(if (fboundp 'ns-find-file)
;;    (global-set-key [ns-drag-file] 'ns-find-file))
;;(setq ns-pop-up-frames nil)

;;; ido-mode
;; built-in alternative to icicles
;;(ido-mode t)
;; Display ido results vertically, rather than horizontally
;;(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
;;(defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
;;(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
;;(load "~/git/emacs/ido-find-resource.el")

;;; key-chord
;;(add-to-list 'load-path "~/.emacs.d/plugins/key-chord")
;;(require 'key-chord)

;;(key-chord-define js-mode-map ";;" "\C-e;")

;;; zencoding
;;(add-to-list 'load-path "~/.emacs.d/plugins/zencoding")
;;(require 'zencoding-mode)
;;(add-hook 'sgml-mode-hook 'zencoding-mode) ;;; Auto-start on any markup modes
;;(global-set-key (kbd "C-c j") 'zencoding-expand-line)

(provide 'my-misc)
