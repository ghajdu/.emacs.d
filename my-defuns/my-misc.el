;;; my-misc.el --- Miscanellous functions

;;; Commentary:

;;; Code:

;; Open drag and drop in same window/instance
(if (fboundp 'ns-find-file)
    (global-set-key [ns-drag-file] 'ns-find-file))
(setq ns-pop-up-frames nil)

(defun copy-full-path-to-kill-ring ()
  "Copy buffer's full path to kill ring."
  (interactive)
  (when buffer-file-name
    (message (buffer-file-name))
    (kill-new (file-truename buffer-file-name))))

;; When you press x, you are prompted whether or not to delete all the
;; marked files. With the top setting (default), you're prompted for
;; each directory individually, and with the always setting you're not
;; prompted (other than the initial prompt).
(defvar dired-recursive-deletes)
(setq dired-recursive-deletes 'always)

(defun mbj/downcase-char-at-point (n)
  "Downcase char at point N."
  (interactive "p")
  (dotimes (i (or n 1))
    (downcase-region (point) (progn (forward-char) (point)))))

(defun mbj/upcase-char-at-point (n)
  "Upcase char at point N."
  (interactive "p")
  (dotimes (i (or n 1))
    (upcase-region (point) (progn (forward-char) (point)))))

(defun ert-t ()
  "Run ert t."
  (interactive)
  (ert t))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
  (delete-other-windows))


(provide 'my-misc)

;;; my-misc.el ends here
